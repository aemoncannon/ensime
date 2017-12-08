// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.rpc

import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.rpc.messages._
import org.scalatest.Matchers._
import org.scalatest._
import spray.json._

import scalaz.deriving

object MessageCompanionsSpec {

  sealed abstract class Message

  sealed abstract class Command extends Message
  @deriving(JsReader, JsWriter)
  final case class UpdateAccountCommand(account: Account) extends Command
  @deriving(JsReader, JsWriter)
  final case class AddTransactionCommand(
    from: Int,
    to: Int,
    value: BigDecimal,
    description: Option[String] = None,
    metadata: Option[JsObject] = None
  ) extends Command {
    require(value >= 0)
  }

  object Commands {
    implicit val updateAccountCommand: RpcCommand[UpdateAccountCommand] =
      RpcCommand[UpdateAccountCommand]("updateAccount")
    implicit val addTransactionCommand: RpcCommand[AddTransactionCommand] =
      RpcCommand[AddTransactionCommand]("addTransaction")
  }

  object Command extends CommandCompanion[Command] {
    import Commands._

    val commands = Seq(
      updateAccountCommand,
      addTransactionCommand
    )
  }

  sealed abstract class Response                   extends Message
  case object UpdateAccountResponse                extends Response
  final case class AddTransactionResponse(id: Int) extends Response
  object AddTransactionResponse {
    implicit val jsWriter: JsWriter[AddTransactionResponse] =
      JsWriter[Int].contramap(_.id)
    implicit val jsReader: JsReader[AddTransactionResponse] =
      JsReader[Int].map(AddTransactionResponse(_))
  }

  sealed abstract class Notification extends Message
  @deriving(JsReader, JsWriter)
  final case class AccountUpdatedNotification(account: Account)
      extends Notification
  @deriving(JsReader, JsWriter)
  final case class TransactionAddedNotification(transaction: Transaction)
      extends Notification

  object Notifications {
    implicit val accountUpdatedNotification
      : RpcNotification[AccountUpdatedNotification] =
      RpcNotification[AccountUpdatedNotification]("accountUpdated")
    implicit val transactionAddedNotification
      : RpcNotification[TransactionAddedNotification] =
      RpcNotification[TransactionAddedNotification]("transactionAdded")
  }

  object Notification extends NotificationCompanion[Notification] {
    import Notifications._

    val notifications = Seq(
      accountUpdatedNotification,
      transactionAddedNotification
    )
  }

  @deriving(JsReader, JsWriter)
  final case class Account(id: Int,
                           name: Option[String] = None,
                           metadata: Option[JsObject] = None)

  @deriving(JsReader, JsWriter)
  final case class Transaction(id: Int,
                               from: Int,
                               to: Int,
                               value: BigDecimal,
                               description: Option[String] = None,
                               metadata: Option[JsObject] = None) {
    require(value >= 0)
  }
}

class MessageCompanionsSpec extends FreeSpec {
  import org.ensime.lsp.rpc.MessageCompanionsSpec._

  "A Command" - {
    "with an invalid method" - {
      val jsonRpcRequestMessage = JsonRpcRequestMessage(
        method = "invalidMethod",
        Params(JsObject.empty),
        CorrelationId(1)
      )
      s"will fail to read" in {
        Command.read(jsonRpcRequestMessage) shouldEqual Left(UnknownMethod)
      }
    }
    "of type AddTransactionCommand" - {
      "with params of the wrong type" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "addTransaction",
          Params(JsArray.empty),
          CorrelationId(1)
        )
        s"will fail to read" in {
          Command.read(jsonRpcRequestMessage) shouldEqual Left(NoNamedParams)
        }
      }
      "with empty params" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "addTransaction",
          Params(JsObject.empty),
          CorrelationId(1)
        )
        s"will fail to read" in {
          Command.read(jsonRpcRequestMessage) should matchPattern {
            case Left(OtherError(_)) =>
          }
        }
      }
      val command = AddTransactionCommand(
        from = 0,
        to = 1,
        value = BigDecimal(1000000),
        description = Some("Property purchase"),
        metadata = Some(
          JsObject(
            "property" -> JsString("The TARDIS")
          )
        )
      )
      val id = CorrelationId(1)
      val jsonRpcRequestMessage = JsonRpcRequestMessage(
        method = "addTransaction",
        Params(
          JsObject(
            "from"        -> JsNumber(0),
            "to"          -> JsNumber(1),
            "value"       -> JsNumber(BigDecimal(1000000)),
            "description" -> JsString("Property purchase"),
            "metadata" -> JsObject(
              "property" -> JsString("The TARDIS")
            )
          )
        ),
        CorrelationId(1)
      )
      s"will decode to $command" in {
        Command.read(jsonRpcRequestMessage) shouldEqual
          Right(command)
      }
      s"will encode to $jsonRpcRequestMessage" in {
        import Commands._

        Command.write(command, id) shouldEqual jsonRpcRequestMessage
      }
    }
  }

  "A Response of type AddTransactionResponse" - {
    val addTransactionResponse = AddTransactionResponse(id = 0)
    val id                     = CorrelationId(1)
    val jsonRpcResponseMessage = JsonRpcResponseSuccessMessage(
      JsNumber(0),
      CorrelationId(1)
    )

    s"will decode to $addTransactionResponse" in {
      RpcResponse.read[AddTransactionResponse](jsonRpcResponseMessage) shouldEqual
        Right(addTransactionResponse)
    }
    s"will encode to $jsonRpcResponseMessage" in {
      RpcResponse.write(addTransactionResponse, id) shouldEqual jsonRpcResponseMessage
    }
  }

  "A Notification" - {
    "with an invalid method" - {
      val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
        method = "invalidMethod",
        Params(JsObject.empty)
      )
      s"will fail to read" in {
        Notification.read(jsonRpcNotificationMessage) shouldEqual
          Left(UnknownMethod)
      }
    }
    "of type TransactionAddedNotification" - {
      "with params of the wrong type" - {
        val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
          method = "transactionAdded",
          Params(JsArray.empty)
        )
        s"will fail to read" in {
          Notification.read(jsonRpcNotificationMessage) shouldEqual
            Left(NoNamedParams)
        }
      }
      "with empty params" - {
        val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
          method = "transactionAdded",
          Params(JsObject.empty)
        )
        s"will fail to read" in {
          Notification.read(jsonRpcNotificationMessage) should matchPattern {
            case Left(OtherError(_)) =>
          }
        }
      }
      val notification = TransactionAddedNotification(
        Transaction(
          id = 0,
          from = 0,
          to = 1,
          value = BigDecimal(1000000)
        )
      )
      val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
        method = "transactionAdded",
        params = Params(
          JsObject(
            "transaction" -> JsObject(
              "id"    -> JsNumber(0),
              "from"  -> JsNumber(0),
              "to"    -> JsNumber(1),
              "value" -> JsNumber(1000000)
            )
          )
        )
      )
      s"will decode to $notification" in {
        Notification.read(jsonRpcNotificationMessage) shouldEqual
          Right(notification)
      }
      s"will encode to $jsonRpcNotificationMessage" in {
        import Notifications._

        Notification.write(notification) shouldEqual
          jsonRpcNotificationMessage
      }
    }
  }
}
