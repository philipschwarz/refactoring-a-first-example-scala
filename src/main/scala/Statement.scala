import java.text.NumberFormat
import java.util.{Currency, Locale}

case class Play(name: String, `type`: String)

case class Performance(playID: String, audience: Int)

case class Invoice(customer: String, performances: List[Performance])

def statement(invoice: Invoice, plays: Map[String, Play]): String =

  def amountFor(aPerformance: Performance, play: Play): Int =
    var result = 0
    play.`type` match
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
        throw IllegalArgumentException(s"unknown type ${play.`type`}")
    result

  def playFor(aPerformance: Performance): Play =
    plays(aPerformance.playID)

  var totalAmount = 0
  var volumeCredits = 0
  var result = s"Statement for ${invoice.customer}\n"
  val formatter = NumberFormat.getCurrencyInstance(Locale.US)
  formatter.setCurrency(Currency.getInstance(Locale.US))
  
  for (perf <- invoice.performances)
    val play = playFor(perf)
    var thisAmount = amountFor(perf,play)

    // add volume credits
    volumeCredits += math.max(perf.audience - 30, 0)
    // add extra credit for every ten comedy attendees
    if "comedy" == play.`type` then volumeCredits += math.floor(perf.audience / 5).toInt

    // print line for this order
    result += s"  ${play.name}: ${formatter.format(thisAmount/100)} (${perf.audience} seats)\n"
    totalAmount += thisAmount
  end for

  result += s"Amount owed is ${formatter.format(totalAmount/100)}\n"
  result += s"You earned $volumeCredits credits\n"
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