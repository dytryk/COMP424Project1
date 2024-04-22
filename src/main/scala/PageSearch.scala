import scala.math.{log, log10}
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: implement this method and remove this stub
        pages.map(page => query.count(page.text.contains(_)))
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms
     *              in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: implement this method and remove this stub
//        val texts = pages.map(_.text)
//
//        def TF(q: String, text: String): Double = text.split("\\W+").count(_ == q).toDouble
//
//        query.map(q => texts.map(text => TF(q, text)).sum)
        val texts = pages.map(_.text)

        query.map(q => texts.map(text => text.split("\\W+").count(_ == q).toDouble).sum)

    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: implement this method and remove this stub
        def IDF[A](s: A): Double = pages.count(_.text.contains(s))
        val ls: List[Double] = List(1.0, 2.0, 3.0)
        ls
//        pages.map(page => (query.count(q => page.text.contains(q)) / page.text.length) * (log10(pages.length / (1 + IDF(q)))))
    }
}