//Mateusz Nasewicz

//Zadanie 1
class MyPair[A,B](var fst: A, var snd: B):
  override def toString = s"($fst, $snd)"

val pair = MyPair(123, "abc")
pair.toString() == "(123, abc)"
pair.fst == 123
pair.snd == "abc"

pair.fst = 321
pair.snd = "cba"
pair.fst == 321
pair.snd == "cba"
pair.toString() == "(321, cba)"

//Zadanie 2
class BankAccount(initialBalance : Double):
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
  override def toString = "%.2f".format(balance)

//Zadanie 2a
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance):
  override def deposit(amount: Double) = super.deposit(amount - 1)
  override def withdraw(amount: Double) = super.withdraw(amount + 1)

val checkingAccount = CheckingAccount(100)
checkingAccount.checkBalance == 100
checkingAccount.withdraw(9) == 90
checkingAccount.deposit(11) == 100

//Zadanie 2b
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance):
  private var transactionCounter = 0

  def earnMonthlyInterest(): Unit =
    transactionCounter = 0
    super.deposit(checkBalance * 0.01)

  override def deposit(amount: Double) =
    transactionCounter += 1
    var newAmount = amount
    if transactionCounter > 3 then newAmount -= 1
    super.deposit(newAmount)

  override def withdraw(amount: Double) =
    transactionCounter += 1
    var newAmount = amount
    if transactionCounter > 3 then newAmount += 1
    super.withdraw(newAmount)

val savingsAccount = SavingsAccount(100)
savingsAccount.deposit(25)
savingsAccount.withdraw(25)
savingsAccount.deposit(0)
savingsAccount.checkBalance == 100
savingsAccount.deposit(3) == 102
savingsAccount.withdraw(1) == 100
savingsAccount.earnMonthlyInterest()
savingsAccount.checkBalance == 101

val savingsAccountDebt = SavingsAccount(-100)
savingsAccountDebt.earnMonthlyInterest()
savingsAccountDebt.checkBalance == -101

//Zadanie 3a
abstract class Zwierz(imie: String):
  def dajGlos: String
  def rodzaj = getClass.getSimpleName
  override def toString = s"$rodzaj $imie daje głos $dajGlos"

//Zadanie 3b
class Pies(imie: String = "bez imienia") extends Zwierz(imie):
  override def dajGlos: String = "Hau"

class Kot(imie: String = "bez imienia") extends Zwierz(imie):
  override def dajGlos: String = "Miau"

//Zadanie 3c
object TestZwierza:
  def main(args: Array[String]): Unit =
    for zwierz <- Vector(Pies("Pluto"),Kot()) do
      println(zwierz)

TestZwierza.main(Array())