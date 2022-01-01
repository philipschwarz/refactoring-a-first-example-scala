import java.text.NumberFormat
import java.util.{Currency, Locale}

case class Play(name: String, `type`: String)

case class Performance(playID: String, audience: Int)

case class Invoice(customer: String, performances: List[Performance])

case class StatementData(customer: String, performances: List[Performance])

def statement(invoice: Invoice, plays: Map[String, Play]): String =
  val statementData = StatementData(invoice.customer,invoice.performances)
  renderPlainText(statementData,plays)

def renderPlainText(data: StatementData, plays: Map[String, Play]): String =

  def amountFor(aPerformance: Performance): Int =
    var result = 0
    playFor(aPerformance).`type` match
      case "tragedy" =>
        result = 40_000
        if aPerformance.audience > 30
        then result += 1_000 * (aPerformance.audience - 30)
      case "comedy" =>
        result = 30_000
        if aPerformance.audience > 20
        then result += 10_000 + 500 * (aPerformance.audience - 20)
        result += 300 * aPerformance.audience
      case other =>
        throw IllegalArgumentException(s"unknown type ${playFor(aPerformance).`type`}")
    result

  def playFor(aPerformance: Performance): Play =
    plays(aPerformance.playID)

  def volumeCreditsFor(aPerformance: Performance): Int =
    var result = 0
    result += math.max(aPerformance.audience - 30, 0)
    if "comedy" == playFor(aPerformance).`type` then result += math.floor(aPerformance.audience / 5).toInt
    result

  def usd(aNumber: Int): String =
    val formatter = NumberFormat.getCurrencyInstance(Locale.US)
    formatter.setCurrency(Currency.getInstance(Locale.US))
    formatter.format(aNumber)

  def totalVolumeCredits: Int =
    var result = 0
    for (perf <- data.performances)
      result += volumeCreditsFor(perf)
    result

  def totalAmount: Int =
    var result = 0
    for (perf <- data.performances)
      result += amountFor(perf)
    result

  var result = s"Statement for ${data.customer}\n"
  for (perf <- data.performances)
    result += s"  ${playFor(perf).name}: ${usd(amountFor(perf) /100)} (${perf.audience} seats)\n"

  result += s"Amount owed is ${usd(totalAmount/100)}\n"
  result += s"You earned $totalVolumeCredits credits\n"
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