import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map{case (s, w) => s -> 1.0}
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight that is a simple count of the
     *              number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        val allLinks = pages.values.flatMap(_.links)
        pages.map { case (s, _) => s -> allLinks.count(_ == s).toDouble }
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: remove this stub and implement this method
        // Simulate 10,000 random walks for 100 steps each
        val numWalks = 10000
      val numSteps = 100
      val dampingFactor = 0.85

      // Initialize page counts to 0 for all pages
      var pageCounts: Map[String, Int] = pages.keys.map(_ -> 0).toMap

      // Perform random walks
      for (_ <- 1 to numWalks) {
        var currentPage = pages.values.toList(new Random().nextInt(pages.size))
        for (_ <- 1 to numSteps) {
          val possibleLinks = currentPage.links
          if (possibleLinks.nonEmpty && new Random().nextDouble() < dampingFactor) {
            val nextLink = possibleLinks(new Random().nextInt(possibleLinks.size))
            currentPage = pages.getOrElse(nextLink, currentPage)
          } else {
            currentPage = pages.values.toList(new Random().nextInt(pages.size))
          }
        }
        // Update page counts based on the end page of the random walk
        pageCounts += currentPage.id -> (pageCounts.getOrElse(currentPage.id, 0) + 1)
      }

      // Normalize the page counts to get PageRank values
      val totalWalks = numWalks + pages.size
      val totalNumPages = pages.size
      val pageRanks = pageCounts.mapValues(count => (count + 1).toDouble / (totalWalks + totalNumPages).toDouble)

      pageRanks.toMap




//        @tailrec
//        def walk(pages: Map[String, WebPage], page: WebPage, steps: Int = 0): WebPage = {
//            if (steps == 100) page
//            else {
//                val possibleLinks = page.links
//                val numLinks = possibleLinks.size
//                val allPages = pages.values.toList
//
//                val rand = new Random()
//
//                val chanceToFollowLink = rand.nextInt(100)
//                val whereToJump = rand.nextInt(allPages.size)
//
//                val nextPage = if (numLinks > 0 && chanceToFollowLink < 85) {
//                    val whichLink = rand.nextInt(numLinks)
//                    pages.get(possibleLinks(whichLink)).getOrElse(allPages(rand.nextInt(allPages.size)))
//                } else {
//                    allPages(whereToJump)
//                }
//
//                walk(pages, nextPage, steps + 1)
//            }
//        }
//
//        // Initialize the initial page counts to 0 for all pages
//        var pageCounts: Map[String, Int] = pages.keys.map(_ -> 0).toMap
//
//        // Simulate 10,000 random walks for 100 steps each
//        val numWalks = 10000
//        for (_ <- 1 to numWalks) {
//            val startPage = pages.values.toList(new Random().nextInt(pages.size))
//            val endPage = walk(pages, startPage)
//
//            // Update the pageCounts based on the endPage
//            pageCounts += endPage.id -> (pageCounts.getOrElse(endPage.id, 0) + 1)
//        }
//
//        // Normalize the pageCounts to get the PageRank values
//        val totalWalks = numWalks + pages.size
//        val pageRanks = pageCounts.mapValues(count => (count + 1).toDouble / totalWalks.toDouble).toMap
//
//        pageRanks






//        @tailrec
//        def walk(pages: Map[String, WebPage], page: WebPage, i: Int = 0): WebPage = {
//            if i == 100 then return page
//
//            val possLinks = page.links
//            val numLinks = possLinks.length
//            val numPages = pages.size
//            val listPages = pages.values.toList
//
//            val rand1 = new Random()
//            val chanceToWalk = rand1.nextInt(100)
//            val rand2 = new Random()
//            val whereToJump = rand2.nextInt(numPages)
//            val rand3 = new Random()
//            val whichLink = rand3.nextInt(numLinks)
//
//            def getPage(s: String): WebPage = {
//                val allPages = loadWebPages()
//                allPages.find(_.id == s).getOrElse(page)
//            }
//
//            if !page.links.equals("") && chanceToWalk < 85 then walk(pages, pages.get(possLinks(whichLink)).getOrElse(getPage(possLinks(whichLink))), i+1)
//            else walk(pages, listPages(whereToJump), i+1)
//        }
//
//        def getWeight(page: WebPage): Double = {
//            val webPages = (for i <- 0 until 10000 yield walk(pages, pages.head._2)).toList
//            val numTimes = webPages.count(_.id == page.id)
//            (1 + numTimes) / (10000 + pages.size)
//        }
//
//        pages.map{case(key, value) => key -> getWeight(value)}
    }
}