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

  class ChannelController(ref: Ref[Map[String, Channel[WebSocketFrame]]]) {
    def add(ch: Channel[WebSocketFrame]): UIO[Unit] = ref.update(_.updated(ch.id, ch))
    def remove(ch: Channel[WebSocketFrame]): UIO[Unit] = ref.update(_.removed(ch.id))
    def accessAll[A](f: Channel[WebSocketFrame] => Task[A]): Task[List[A]] = {
      for {
        chs <- ref.get.map(_.values.toList)
        as <- ZIO.foreach(chs)(f)
      } yield as
    }
  }
  object ChannelController {
    val make: ZIO[Any, Nothing, ChannelController] = for {
      ref <- Ref.make(Map.empty)
    } yield new ChannelController(ref)
    val layer: ZLayer[Any, Nothing, ChannelController] = ZLayer.fromZIO(make)
  }

  private val socket: Http[ChannelController, Throwable, WebSocketChannelEvent, Unit] = {

    Http.collectZIO[WebSocketChannelEvent] {
      case ChannelEvent(ch, UserEventTriggered(UserEvent.HandshakeComplete)) =>
        for {
          cc <- ZIO.service[ChannelController]
          _ <- Console.printLine(s"${ch.id}: HandshakeComplete")
          msg = s"Hello, ${ch.id}!"
          _ <- cc.accessAll(c => c.writeAndFlush(WebSocketFrame.text(msg)))
          _ <- cc.add(ch)
          _ <- ch.writeAndFlush(WebSocketFrame.text("Hello!"))
        } yield ()

      case ChannelEvent(ch, ChannelUnregistered) =>
        for {
          cc <- ZIO.service[ChannelController]
          _ <- Console.printLine(s"${ch.id}: ChannelUnregistered")
          _ <- cc.remove(ch)
          msg = s"See you, ${ch.id}!"
          _ <- cc.accessAll(c => c.writeAndFlush(WebSocketFrame.text(msg)))
        } yield ()

      case ChannelEvent(ch, ChannelRead(WebSocketFrame.Text(text))) =>
        for {
          cc <- ZIO.service[ChannelController]
          msg = s"${ch.id} says: $text"
          _ <- Console.printLine(s"${ch.id} says: $text")
          _ <- cc.accessAll(c => c.writeAndFlush(WebSocketFrame.text(msg)).when(c.id != ch.id))
        } yield ()

      case ChannelEvent(ch, ExceptionCaught(cause)) =>
        Console.printLine(s"${ch.id}: Channel error!: ${cause.getMessage}")

      case ChannelEvent(ch, _) =>
        Console.printLine(s"${ch.id}: Unknown message! (binary message?)")
    }
  }

  val app: HttpApp[ChannelController, Nothing] = Http.collectZIO[Request] {
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
      ChannelController.layer
    )
  }
}
