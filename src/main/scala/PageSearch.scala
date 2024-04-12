import scala.math.log
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
//        pages.map(p => query.count(q => p.name.contains(q)) + query.count(q => p.url.contains(q)) + query.count(q => p.text.contains(q)) + query.count(q => p.links.contains(q)))
        pages.map(page => query.count(page.text.contains(_)))
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }
}