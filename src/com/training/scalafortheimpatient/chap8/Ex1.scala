package com.training.scalafortheimpatient.chap8

import com.training.scalafortheimpatient.chap8.Ex1.ChargedBankAccount

object Ex1 {
  class BankAccount(val initialBalance : Double) {
    private var balance = initialBalance
    def currentBalance = balance
    def deposit(amount: Double) = { balance += amount; this}
    def withdraw(amount: Double) = { balance -= amount; this}
  }

  class ChargedBankAccount(initialBalance : Double) extends BankAccount(initialBalance) {
    override def deposit(amount: Double) = {
      super.withdraw(1)
      super.deposit(amount)
      this
    }
    override def withdraw(amount: Double) = {
      super.withdraw(1)
      super.withdraw(amount)
      this
    }
  }

  class SavingAccount(initialBalance : Double) extends BankAccount(initialBalance) {
    var freeTransactionLeft = 3
    val interestRate = 0.2

    def earnMonthlyInterest = {
      freeTransactionLeft = 3
      super.deposit(currentBalance * interestRate)
    }

    override def deposit(amount: Double) = {
      super.deposit(amount)
      freeTransactionLeft -= 1
      this
    }
    override def withdraw(amount: Double) = {
      super.withdraw(amount)
      freeTransactionLeft -= 1
      this
    }
  }
}

object Ex1Test extends App {
  private val account: ChargedBankAccount = new ChargedBankAccount(100)
  println(account.withdraw(1).deposit(1).currentBalance + " == 98.0")
}
