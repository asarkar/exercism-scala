// https://twitter.github.io/scala_school/concurrency.html
class BankAccount(var balance: Int = 0, @volatile var open: Boolean = true):
  def closeAccount(): Unit = open = false

  def getBalance: Option[Int] = Option.when(open)(balance)

  def incrementBalance(increment: Int): Option[Int] =
    this.synchronized:
      val bal = getBalance.map(_ + increment)
      bal.foreach(balance = _)
      bal

object Bank:
  def openAccount(): BankAccount = new BankAccount()
