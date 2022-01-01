import java.text.NumberFormat
import java.util.{Currency, Locale}

def statement(invoice: Invoice, plays: Map[String, Play]): String =
  renderPlainText(createStatementData(invoice,plays))

def renderPlainText(data: StatementData): String =

  def usd(aNumber: Int): String =
    val formatter = NumberFormat.getCurrencyInstance(Locale.US)
    formatter.setCurrency(Currency.getInstance(Locale.US))
    formatter.format(aNumber)

  var result = s"Statement for ${data.customer}\n"
  for (perf <- data.performances)
    result += s"  ${perf.play.name}: ${usd(perf.amount/100)} (${perf.audience} seats)\n"

  result += s"Amount owed is ${usd(data.totalAmount/100)}\n"
  result += s"You earned ${data.totalVolumeCredits} credits\n"
  result

val invoices: List[Invoice] = List(
  Invoice( customer = "BigCo",
           performances = List(Performance(playID = "hamlet",  
                                           audience = 55),
                               Performance(playID = "as-like", 
                                           audience = 35),
                               Performance(playID = "othello", 
                                           audience = 40)))
)

val plays = Map (
  "hamlet"  -> Play(name = "Hamlet", `type` = "tragedy"),
  "as-like" -> Play(name = "As You Like It", `type` = "comedy"),
  "othello" -> Play(name = "Othello", `type` = "tragedy")
)

@main def main(): Unit =
  assert(
    statement(invoices(0), plays)
    ==
    """|Statement for BigCo
       |  Hamlet: $650.00 (55 seats)
       |  As You Like It: $580.00 (35 seats)
       |  Othello: $500.00 (40 seats)
       |Amount owed is $1,730.00
       |You earned 47 credits
       |""".stripMargin
  )