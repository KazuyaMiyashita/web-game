import zio.*
import zio.http.*
import zio.http.ChannelEvent.{
  ChannelRead,
  ChannelRegistered,
  ChannelUnregistered,
  ExceptionCaught,
  UserEvent,
  UserEventTriggered
}
import zio.http.model.Method
import zio.http.socket.{WebSocketChannelEvent, WebSocketFrame}

object Main extends ZIOAppDefault {

  class WordBasketController(
      basket: Ref[WordBasket],
      users: Ref[Map[String, Channel[WebSocketFrame]]]
  ) {
    def addUser(ch: Channel[WebSocketFrame]): Task[Unit] = {
      for {
        _ <- users.update(_.updated(ch.id, ch))
        b <- basket.get
        _ <- ch.writeAndFlush(WebSocketFrame.text(s"next: ${b.next}"))
      } yield ()

    }
    def removeUser(ch: Channel[WebSocketFrame]): UIO[Unit] = users.update(_.removed(ch.id))
    def registerWord(ch: Channel[WebSocketFrame], word: String): Task[Unit] = {
      for {
        res <- basket.modify(b => {
          val res = b.accept(word)
          (res, res.getOrElse(b))
        })
        b <- basket.get
        _ <- res match {
          case Left(_) => ch.writeAndFlush(WebSocketFrame.text(s"illegal word! next: ${b.next}"))
          case Right(bas) =>
            ch.writeAndFlush(WebSocketFrame.text(s"OK! next: ${b.next}")) *>
              accessAll(c =>
                c.writeAndFlush(WebSocketFrame.text(s"other person answered: $word, next: ${bas.next}"))
                  .when(c.id != ch.id)
              )
        }
      } yield ()
    }

    def accessAll[A](f: Channel[WebSocketFrame] => Task[A]): Task[List[A]] = {
      for {
        chs <- users.get.map(_.values.toList)
        as <- ZIO.foreach(chs)(f)
      } yield as
    }
  }

  object WordBasketController {
    val make: ZIO[Any, Nothing, WordBasketController] = for {
      basket <- Ref.make(WordBasket.create())
      users <- Ref.make(Map.empty)
    } yield new WordBasketController(basket, users)
    val layer: ZLayer[Any, Nothing, WordBasketController] = ZLayer.fromZIO(make)
  }

  private val socket: Http[WordBasketController, Throwable, WebSocketChannelEvent, Unit] = {

    Http.collectZIO[WebSocketChannelEvent] {
      case ChannelEvent(ch, UserEventTriggered(UserEvent.HandshakeComplete)) =>
        for {
          cc <- ZIO.service[WordBasketController]
          _ <- Console.printLine(s"${ch.id}: HandshakeComplete")
          msg = s"Hello, ${ch.id}!"
          _ <- cc.accessAll(c => c.writeAndFlush(WebSocketFrame.text(msg)))
          _ <- cc.addUser(ch)
          _ <- ch.writeAndFlush(WebSocketFrame.text("Hello!"))
        } yield ()

      case ChannelEvent(ch, ChannelUnregistered) =>
        for {
          cc <- ZIO.service[WordBasketController]
          _ <- Console.printLine(s"${ch.id}: ChannelUnregistered")
          _ <- cc.removeUser(ch)
          msg = s"See you, ${ch.id}!"
          _ <- cc.accessAll(c => c.writeAndFlush(WebSocketFrame.text(msg)))
        } yield ()

      case ChannelEvent(ch, ChannelRead(WebSocketFrame.Text(text))) =>
        for {
          cc <- ZIO.service[WordBasketController]
          msg = s"${ch.id} says: $text"
          _ <- Console.printLine(s"${ch.id} says: $text")
          _ <- cc.registerWord(ch, text)
        } yield ()

      case ChannelEvent(ch, ExceptionCaught(cause)) =>
        Console.printLine(s"${ch.id}: Channel error!: ${cause.getMessage}")

      case ChannelEvent(ch, _) =>
        Console.printLine(s"${ch.id}: Unknown message! (binary message?)")
    }
  }

  val app: HttpApp[WordBasketController, Nothing] = Http.collectZIO[Request] {
    case Method.GET -> !! / "text"          => ZIO.succeed(Response.text("Hello World!"))
    case Method.GET -> !! / "subscriptions" => socket.toSocketApp.toResponse
  }

  override val run: ZIO[Any, Throwable, Nothing] = {
    val port = 8080
    val config = ServerConfig.default.port(port)

    (for {
      server <- Server.serve(app)
    } yield server).provide(
      ServerConfig.live(config),
      Server.live,
      WordBasketController.layer
    )
  }
}
