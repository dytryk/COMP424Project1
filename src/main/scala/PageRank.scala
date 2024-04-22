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
        // TODO: remove this stub and implement this method

        val allLinks = pages.flatMap(_._2.links)
        pages.map { case (s, page) => s -> allLinks.count(_ == s).toDouble }
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: remove this stub and implement this method

        @tailrec
        def walk(pages: Map[String, WebPage], page: WebPage, i: Int = 0): WebPage = {
            if i == 100 then return page
            
            val possLinks = page.links
            val numLinks = possLinks.length
            val numPages = pages.size
            val listPages = pages.values.toList

            val rand1 = new Random()
            val chanceToWalk = rand1.nextInt(100)
            val rand2 = new Random()
            val whereToJump = rand2.nextInt(numPages)
            val rand3 = new Random()
            val whichLink = rand3.nextInt(numLinks)
            
            def getPage(s: String): WebPage = {
                val allPages = loadWebPages()
                allPages.find(_.id == s).getOrElse(page)
            }
            
            if !page.links.equals("") && chanceToWalk < 85 then walk(pages, pages.get(possLinks(whichLink)).getOrElse(getPage(possLinks(whichLink))), i+1)
            else walk(pages, listPages(whereToJump), i+1)
        }
        
        def getWeight(page: WebPage): Double = {
            val webPages = (for i <- 0 until 10000 yield walk(pages, pages.head._2)).toList
            val numTimes = webPages.count(_.id == page.id)
            (1 + numTimes) / (10000 + pages.size)
        }
        
        pages.map{case(key, value) => key -> getWeight(value)}
    }
}