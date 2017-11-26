// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.net._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

import org.ensime.AkkaBackCompat
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.api.EnsimeFile.Implicits.DefaultCharset
import org.ensime.util.path._

class ServerStartupSpec
    extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with AkkaBackCompat {

  val original = EnsimeConfigFixture.EmptyTestProject

  "Server" should "start up and bind to random ports" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        system.actorOf(ServerActor.props(), "ensime-main")

        eventually(timeout(scaled(10 seconds)), interval(scaled(1 second))) {
          PortUtil.port(config.cacheDir.file, "http").isDefined
        }
      }
    }
  }

  it should "start up and bind to preferred ports" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        // this can fail randomly. No general solution.
        val preferredHttp = 10001

        (config.cacheDir.file / "http").write(preferredHttp.toString)

        system.actorOf(ServerActor.props(), "ensime-main")

        eventually(timeout(scaled(10 seconds)), interval(scaled(1 second))) {
          val http = new Socket

          try {
            http.connect(new InetSocketAddress("127.0.0.1", preferredHttp))
            http.isConnected()
          } finally {
            Try(http.close())
          }
        }
      }
    }
  }

  it should "shutdown if preferred HTTP port is not available" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        val preferredHttp = 10003
        (config.cacheDir.file / "http").write(preferredHttp.toString)

        val socket = new ServerSocket()

        try {
          val httpHog =
            socket.bind(new InetSocketAddress("127.0.0.1", preferredHttp))

          eventually { assert(socket.isBound()) }

          system.actorOf(ServerActor.props(), "ensime-main")

          Await.result(system.whenTerminated, akkaTimeout.duration)
        } finally {
          socket.close()
        }
      }
    }
  }

}
