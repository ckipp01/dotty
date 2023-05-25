import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.stream.ActorMaterializer
import scala.io.StdIn

object WebServer:
  def main(args: Array[String]): Unit =
    val x = ContentTypes.`text/html(UTF-8)`
