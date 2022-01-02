def createStatementData(invoice: Invoice, plays: Map[String,Play]): StatementData =

  def enrichPerformance(aPerformance: Performance): EnrichedPerformance =
    val calculator = PerformanceCalculator(aPerformance,playFor(aPerformance))
    EnrichedPerformance(
      aPerformance.playID,
      calculator.play,
      aPerformance.audience,
      calculator.amount,
      volumeCreditsFor(aPerformance))

  def playFor(aPerformance: Performance): Play =
    plays(aPerformance.playID)

  def volumeCreditsFor(aPerformance: Performance): Int =
    PerformanceCalculator(aPerformance,playFor(aPerformance)).volumeCredits

  def totalVolumeCredits(performances: List[EnrichedPerformance]): Int =
    performances.foldLeft(0)((total,perf) => total + perf.volumeCredits)

  def totalAmount(performances: List[EnrichedPerformance]): Int =
    performances.foldLeft(0)((total,perf) => total + perf.amount)

  val enrichedPerformances = invoice.performances.map(enrichPerformance)
  StatementData(invoice.customer,
    enrichedPerformances,
    totalAmount(enrichedPerformances),
    totalVolumeCredits(enrichedPerformances))