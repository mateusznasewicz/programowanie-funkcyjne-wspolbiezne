//Mateusz Nasewicz

//Zadanie 1
class MyPair[A, B](var fst: A, var snd: B):
  override def toString = s"($fst, $snd)"

val a = MyPair(1, "a")
a.fst == 1
a.snd == "a"
a.toString == "(1, a)"

//Zadanie 2
class BankAccount(initialBalance: Double):
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount: Double) = {
    balance += amount; balance
  }
  def withdraw(amount: Double) = {
    balance -= amount; balance
  }
  override def toString = "%.2f".format(balance)

//Zadanie 2a
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance):
  override def deposit(amount: Double) = super.deposit(amount - 1)
  override def withdraw(amount: Double) = super.withdraw(amount + 1)

//Zadanie 2b
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance):
  private var transactionsThisMonth = 0

  def earnMonthlyInterest(): Unit =
    transactionsThisMonth = 0
    super.deposit(checkBalance * 0.01)

  override def deposit(amount: Double): Double =
    transactionsThisMonth += 1
    super.deposit(if transactionsThisMonth <= 3 then amount else amount - 1)

  override def withdraw(amount: Double): Double =
    transactionsThisMonth += 1
    super.deposit(if transactionsThisMonth <= 3 then amount else amount + 1)

//Zadanie 3
abstract class Zwierz(val imie: String = "bez imienia"):
  def dajGlos: String
  def rodzaj: String = getClass.getSimpleName
  override def toString: String = s"$rodzaj $imie daje głos $dajGlos!"

//Zadanie 3a
class Pies(imie: String = "bez imienia") extends Zwierz(imie):
  override def dajGlos = "Hau"

class Kot(imie: String = "bez imienia") extends Zwierz(imie):
  override def dajGlos = "Miau"

class Koza(imie: String = "bez imienia") extends Zwierz(imie):
  override def dajGlos = "Mee"

object TestZwierza:
  def main(args: Array[String]): Unit =
    val zwierzeta = Vector(Pies("pluto"), Kot("rudy"), Koza())
    for zwierz <- zwierzeta do println(zwierz)

TestZwierza.main(Array())