import java.text.NumberFormat
import java.util.{Currency, Locale}

def statement(invoice: Invoice, plays: Map[String, Play]): String =
  renderPlainText(createStatementData(invoice,plays))

def renderPlainText(data: StatementData): String =

  var result = s"Statement for ${data.customer}\n"
  for (perf <- data.performances)
    result += s"  ${perf.play.name}: ${usd(perf.amount/100)} (${perf.audience} seats)\n"

  result += s"Amount owed is ${usd(data.totalAmount/100)}\n"
  result += s"You earned ${data.totalVolumeCredits} credits\n"
  result

def htmlStatement(invoice: Invoice, plays: Map[String, Play]): String =
  renderHtml(createStatementData(invoice,plays))

def renderHtml(data: StatementData): String =
  var result = s"<h1>Statement for ${data.customer}</h1>\n"
  result += "<table>\n"
  result += "<tr><th>play</th><th>seats</th><th>cost</th></tr>\n"
  for (perf <- data.performances)
    result += s"<tr><td>${perf.play.name}</td><td>${perf.audience}</td>"
    result += s"<td>${usd(perf.amount/100)}</td></tr>\n"
  result += "</table>\n"
  result += s"<p>Amount owed is <em>${usd(data.totalAmount/100)}</em></p>\n"
  result += s"<p>You earned <em>${data.totalVolumeCredits}</em> credits</p>\n"
  result

def usd(aNumber: Int): String =
  val formatter = NumberFormat.getCurrencyInstance(Locale.US)
  formatter.setCurrency(Currency.getInstance(Locale.US))
  formatter.format(aNumber)

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
  assert(
    htmlStatement(invoices(0), plays)
    ==
    """|<h1>Statement for BigCo</h1>
       |<table>
       |<tr><th>play</th><th>seats</th><th>cost</th></tr>
       |<tr><td>Hamlet</td><td>55</td><td>$650.00</td></tr>
       |<tr><td>As You Like It</td><td>35</td><td>$580.00</td></tr>
       |<tr><td>Othello</td><td>40</td><td>$500.00</td></tr>
       |</table>
       |<p>Amount owed is <em>$1,730.00</em></p>
       |<p>You earned <em>47</em> credits</p>
       |""".stripMargin
   )
