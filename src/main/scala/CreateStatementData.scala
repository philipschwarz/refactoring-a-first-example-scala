def createStatementData(invoice: Invoice, plays: Map[String,Play]): StatementData =

  def enrichPerformance(aPerformance: Performance): EnrichedPerformance =
    val calculator = PerformanceCalculator(aPerformance,playFor(aPerformance))
    EnrichedPerformance(
      aPerformance.playID,
      calculator.play,
      aPerformance.audience,
      calculator.amount,
      calculator.volumeCredits)

  def playFor(aPerformance: Performance): Play =
    plays(aPerformance.playID)

  def totalVolumeCredits(performances: List[EnrichedPerformance]): Int =
    performances.map(_.volumeCredits).sum

  def totalAmount(performances: List[EnrichedPerformance]): Int =
    performances.map(_.amount).sum

  val enrichedPerformances = invoice.performances.map(enrichPerformance)
  StatementData(invoice.customer,
    enrichedPerformances,
    totalAmount(enrichedPerformances),
    totalVolumeCredits(enrichedPerformances))