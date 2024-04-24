import scala.math.{log, log10}
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map { page =>
            val queryCount = query.count { term =>
                page.text.contains(term)
            }
            queryCount.toDouble
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms
     *              in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map { page =>
            val termCount = query.count { term =>
                page.text.contains(term)
            }
            termCount.toDouble / page.text.length.toDouble
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val tfs = tf(pages, query)

        pages.zip(tfs).map { case (page, tf) =>
            val idfs = query.map { term =>
                math.log10(pages.size.toDouble / (1 + pages.count(_.text.contains(term))))
            }
            tf * idfs.sum
        }
    }
}