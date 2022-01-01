def createStatementData(invoice: Invoice, plays: Map[String,Play]): StatementData =

  def enrichPerformance(aPerformance: Performance): EnrichedPerformance =
    EnrichedPerformance(
      aPerformance.playID,
      playFor(aPerformance),
      aPerformance.audience,
      amountFor(aPerformance),
      volumeCreditsFor(aPerformance))

  def playFor(aPerformance: Performance): Play =
    plays(aPerformance.playID)

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

  def volumeCreditsFor(aPerformance: Performance): Int =
    var result = 0
    result += math.max(aPerformance.audience - 30, 0)
    if "comedy" == playFor(aPerformance).`type` then result += math.floor(aPerformance.audience / 5).toInt
    result

  def totalVolumeCredits(performances: List[EnrichedPerformance]): Int =
    performances.foldLeft(0)((total,perf) => total + perf.volumeCredits)

  def totalAmount(performances: List[EnrichedPerformance]): Int =
    performances.foldLeft(0)((total,perf) => total + perf.amount)

  val enrichedPerformances = invoice.performances.map(enrichPerformance)
  StatementData(invoice.customer,
    enrichedPerformances,
    totalAmount(enrichedPerformances),
    totalVolumeCredits(enrichedPerformances))