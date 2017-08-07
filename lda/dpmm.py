import numpy as np
import time
import optparse
import vocabulary
from math import lgamma
import pickle
import os.path

verbose = False

class DPMM:
    def __init__(self, T, alpha, beta, doc, V, topic_file, smartinit=True):
        self.n_topics = T
        self.alpha = alpha # parameter of topics prior
        self.beta = beta   # parameter of words prior
        self.doc = doc
        self.V = V
        self.K = 0 # number of clusters
        self.N = len(doc) # number of words
        self.read_topics(topic_file) # A set of fixed topics
        assert(self.phi.shape[1] == V)

        self.z_i = [None] * V # word assignments to clusters
        self.s_k = [] # assignments of topics to clusters

        self.n_k = [] # word count of each cluster
        self.n_k_i = [] # word count of each cluster and vocabulary
        self.n_t_i = np.zeros((T, V)) # word count of each topic and vocabulary
        self.n_t = np.zeros(T) # word count of each topic

    def get_new_topic_for_cluster(self, w):
        F = np.array([self.word_multinomial(t, w) for t in range(self.n_topics)])
        G_0 = np.array(self.prob_under_G_0()).ravel()

        p = np.multiply(F, G_0) / np.sum(np.multiply(F, G_0))

        s_new = np.random.multinomial(1, p).argmax()

        return(s_new)

    def prob_under_G_0(self):
        start = time.time()
        #print("prob_under_G_0")
        psi = 0
        n_samples = 10
        for i in range(n_samples):
            psi += np.random.dirichlet(self.n_t + self.beta)

        end = time.time()
        # print(end - start)
        # if (end - start > 0.001):
        #     print(t)
        #     print(self.n_t)
        return(psi / n_samples)


    def word_multinomial(self, t, w):
        return(self.phi[t,w])

    def read_topics(self, topic_file):
        self.phi = []
        with open(topic_file, 'r') as f:
            self.voca = f.readline().split(",") # skip the vocabulary line
            for t in range(self.n_topics):
                topic = f.readline().split(",")
                topic = [float(i) for i in topic]
                self.phi.append(topic)

        self.phi = np.matrix(self.phi)

    def inference(self):
    #     """learning once iteration"""
        start = time.time()

        for i, w in enumerate(self.doc):
            # print("Word " + str(w))
            # print("new")
            self.remove_sample(i, w)
            
            end = time.time()
            # print("remove sample")
            # print(end - start)
            start = time.time()
            
            new_z = self.draw_assignment(i, w)
            
            end = time.time()
            # print("draw_assignment")
            # print(end - start)
            start = time.time()

            # check if new cluster is created. Clusters are numbered from zero
            if (new_z == self.K ):
                self.handle_new_cluster(i, w)

                end = time.time()
                # print("new_cluster")
                # print(end - start)
                start = time.time()

            else:
                self.z_i[i] = new_z
                self.n_k[new_z] += 1
                self.n_k_i[new_z][w] +=1

                self.n_t_i[ self.s_k[new_z] ][w] += 1
                self.n_t[ self.s_k[new_z] ] += 1

                end = time.time()
                # print("updating counts")
                # print(end - start)
                start = time.time()

        assert(sum(self.n_k) == self.N)
        assert(sum(self.n_t) == self.N)

    def remove_sample(self, i, w):
        # discount for n-th word t with topic z
        z = self.z_i[i]
        if z is None: # Beginning of training, word is not assigned to any cluster
            return

        self.n_k[z] -= 1
        self.n_k_i[z][w] -= 1
        self.n_t_i[ self.s_k[z] ][w] -= 1
        self.n_t[ self.s_k[z] ] -= 1

        if (sum([1 for x in self.n_k if x < 0])):
            print("n_k")
            print(self.n_t )
            print(self.n_k )
            print(self.n_k_i)
            print(self.n_t_i)
            print(z)
            print(self.s_k[z] )
            print(self.s_k)
            raise

        if (sum([1 for x in self.n_t if x < 0])):
            print("n_t")
            print(self.n_t )
            print(self.n_k )
            print(self.n_k_i)
            print(self.n_t_i)
            print(z)
            print(self.s_k[z] )
            print(self.s_k)
            raise


        if self.n_k[z] == 0:
            # re-arrange the clusters so that each of them is non-empty
            # put the last cluster on the place of the cluster z
            self.s_k[z] = self.s_k[self.K-1] 
            self.s_k = self.s_k[:-1]
            
            self.n_k[z] = self.n_k[self.K-1]
            self.n_k = self.n_k[:-1]

            self.n_k_i[z] = self.n_k_i[self.K-1] 
            self.n_k_i = self.n_k_i[:-1]

            # put all the words assigned to the last cluster to be cluster z
            self.z_i = [z if x == (self.K -1) else x for x in self.z_i]

            self.K = self.K - 1

    def draw_assignment(self, i, w):
        p = []
        for k in range(self.K):
            s = self.s_k[k]
            p_topic = self.n_k_i[k][i]/(self.N + self.alpha - 1) * self.word_multinomial(s, w)
            p.append(p_topic)

        F = np.array([self.word_multinomial(t, w) for t in range(self.n_topics)])
        G_0 = np.array(self.prob_under_G_0()).ravel()

        p_new = self.alpha/(self.N + self.alpha - 1)* np.sum(np.multiply(F, G_0))
        p.append(p_new)

        # clusters are numbered from zero
        z_new = np.random.multinomial(1, p).argmax()
        return(z_new)

    def handle_new_cluster(self, i, w):
         # draw topic for the new cluster
        start = time.time()
        
        s_new = self.get_new_topic_for_cluster(w)
        
        end = time.time()
        # print("get_new_topic_for_cluster")
        # print(end-start )
        start = time.time()
        
        # if cluster topic already belongs to another cluster
        if s_new in self.s_k:
            # Merge two clusters: a new one and k_prime which has the same topic
            k_prime = self.s_k.index(s_new)
            self.n_k[k_prime] += 1
            self.n_k_i[k_prime][w] +=1

            self.z_i[i] = k_prime

            self.n_t_i[ s_new ][w] += 1
            self.n_t[ s_new ] += 1

            if (verbose):
                print("Merging clusters")
                print(self.s_k)
                print(self.n_k)

        else:
            # Yeeey! New cluster with a new topics
            self.z_i[i] = self.K 
            self.s_k.append(s_new)

            self.n_k.append(1)
            self.n_k_i.append(np.zeros(self.V))
            #print(np.matrix(self.n_k_i).shape)
            self.n_k_i[self.K ][w] = 1

            self.n_t_i[ self.s_k[self.K ] ][w] += 1
            self.n_t[ self.s_k[self.K ] ] += 1

            self.K = self.K + 1

            if (verbose):
                print("New cluster")
                print(self.s_k)
                print(self.n_k)

        end = time.time()
        # print("handle new cluster: housekeeping")
        # print(end-start )

    def perplexity(self, doc=None):
        if doc == None: doc = self.doc
        log_per = 0

        Tbeta = self.n_topics * self.beta
        psi = self.n_t / (len(self.doc) + Tbeta)
        
        for w in doc:
            if (np.inner(self.phi[:,w].T, psi) <= 0):
                print(self.n_t )

            log_per -= np.log(np.inner(self.phi[:,w].T, psi))
        N = len(doc)
        return np.exp(log_per / N)

def dpmm_learning(dpmm, iteration, voca):
    pre_perp = 0
    for i in range(iteration):
        if (verbose):
            print("New iteration " + str(i))
        dpmm.inference()
        perp = dpmm.perplexity(dpmm.doc)
        n_clusters_min_10 = sum([1 if x > 10 else 0 for x in dpmm.n_k])
        print ("-%d p=%f clusters=%d big_clusters=%d" % (i + 1, perp, dpmm.K, n_clusters_min_10))
        if pre_perp:
            if pre_perp < perp:
                #output_word_topic_dist(dpmm, voca)
                pre_perp = None
            else:
                pre_perp = perp

        # stop if all the clusters have more than 10 words -- then it converged
        if i > 10 and n_clusters_min_10 == dpmm.K:
            return

def main():
    parser = optparse.OptionParser()
    parser.add_option("-f", dest="filename", help="corpus filename")
    parser.add_option("-c", dest="corpus", help="using range of Brown corpus' files(start:end)")
    parser.add_option("--alpha", dest="alpha", type="float", help="parameter alpha", default=0.5)
    parser.add_option("--beta", dest="beta", type="float", help="parameter beta", default=0.5)
    parser.add_option("-t", dest="T", type="int", help="number of topics", default=20)
    parser.add_option("-i", dest="iteration", type="int", help="iteration count", default=100)
    parser.add_option("-s", dest="smartinit", action="store_true", help="smart initialize of parameters", default=False)
    parser.add_option("--stopwords", dest="stopwords", help="exclude stop words", action="store_true", default=False)
    parser.add_option("--seed", dest="seed", type="int", help="random seed")
    parser.add_option("--df", dest="df", type="int", help="threshold of document freaquency to cut words", default=0)
    parser.add_option("--topics", dest="topic_file", type="str", help="CSV file with the topics: columns are words from vocabulary, rows are topics", default="topics.csv")
    (options, args) = parser.parse_args()
    if not (options.filename or options.corpus): parser.error("need corpus filename(-f) or corpus range(-c)")

    start = time.time()
    if options.filename:
        corpus = vocabulary.load_file(options.filename)
    else:
        corpus = vocabulary.load_corpus(options.corpus)
        if not corpus: parser.error("corpus range(-c) forms 'start:end'")
    if options.seed != None:
        np.random.seed(options.seed)

    pickle_file = "docs" + str(options.corpus) + ".pickle"
    if os.path.isfile(pickle_file): 
        voca, docs = pickle.load(open(pickle_file, 'rb'))
    else:
        voca = vocabulary.Vocabulary(options.stopwords)
        docs = [voca.doc_to_ids(doc) for doc in corpus]
        if options.df > 0: docs = voca.cut_low_freq(docs, options.df)

        with open(pickle_file, 'wb') as f:
            pickle.dump([voca, docs], f) 

    #for m, doc in enumerate(docs):
    m = 9
    doc = docs[m]
    #print(" ".join(corpus[m][250:500]))
    print("Document " + str(m))

    dpmm = DPMM(options.T, options.alpha, options.beta, doc, voca.size(), options.topic_file, options.smartinit)
    print ("corpus=%d, vocabSize=%d, wordsInDoc=%d, T=%d, a=%f, b=%f" % (len(corpus), len(voca.vocas), len(doc), options.T, options.alpha, options.beta))

    dpmm_learning(dpmm, options.iteration, voca)

    print("Document " + str(m))
    print("Topics: " + str(dpmm.s_k))
    print("Counts: " + str(dpmm.n_k))

if __name__ == "__main__":
    main()
