#Γ.Ι.Παπαμανώλης
#’σκηση Β

#Ερώτημα Β1

#φορτώνουμε το σύνολο δεδομένων
data(iris)

#αφαιρούμε τη στήλη με τα ονόματα των κλάσεων
iris1=subset(iris, select=-Species)

#εκτελελούμε την kmeans() για k=3
clusters<-kmeans(iris1,3, nstart = 25)

#εμφανίζουμε τα κέντρα
clusters$centers

# εμφανίζουμε τη συστάδα στην οποία ανατέθηκε κάθε δεδομένο του συνόλου 
# iris (με έντονη εκτύπωση ο αύξων αριθμός του δεδομένου στο iris 
# και από κάτω ο αριθμός της συστάδας)

clusters$cluster

#Ερώτημα Β2

# Η συνάρτηση δέχεται ως ορίσματα δύο πίνακες και επιστρέφει έναν πίνακα
# με τις ευκλείδειες αποστάσεις κάθε παρατήρησης:
# O πίνακας points (i x j+1) αποτελείται από i παρατηρήσεις με j γνωρίσματα. 
# Στη στήλη j+1 βρίσκεται ο αύξων αριθμός της συστάδας κάθε παρατήρησης.
# O πίνακας centro (k x j) αποτελείται από τα κέντρα των k συστάδων για 
# κάθε ένα από τα j γνωρίσματα.
# Ο πίνακας distanceMatrix (i) που επιστρέφεται έχει τις ευκλείδειες
# αποστάσεις κάθε παρατήρησης από το κέντρο της συστάδας στην οποία ανήκει.
myEuclid<-function(points, centro) {
    distanceMatrix <- matrix(NA, nrow=dim(points)[1])
    for(i in 1:nrow(points)) {
        distanceMatrix[i]<-0
            for (j in 1:ncol(centro)) {
                distanceMatrix[i] <- distanceMatrix[i] + (points[i,j]-centro[points[i,ncol(points)],j])^2
            }
        distanceMatrix[i] <- sqrt(distanceMatrix[i])
    }
    return(distanceMatrix)
}

#ενσωματώνω τη στήλη με τον αριθμό συστάδας στο σύνολο δεδομένων iris1
iris_clustered<-cbind(iris1,"Cluster"=clusters[["cluster"]])

#υπολογίζω τις ευκλείδιες αποστάσεις κάθε δεδομένου από το κέντρο της συστάδας του με τη συνάρτηση myEuclid()
distances<-myEuclid(iris_clustered, clusters[["centers"]])

#εμφανίζω τα 5 πιο απομακρυσμένα δεδομένα με τις τιμές των γνωρισμάτων τους 
head(iris_clustered[order(-distances),],n=5) 

#Ερώτημα Β3 

plot(subset(iris1$Sepal.Length, clusters$cluster==1), subset(iris1$Sepal.Width, clusters$cluster==1) , col="red", xlab="Sepal Length", ylab="Sepal Width", xlim=c(4.2,8), ylim=c(1.8,4.5))
points(subset(iris1$Sepal.Length, clusters$cluster==2), subset(iris1$Sepal.Width, clusters$cluster==2) , col="blue")
points(subset(iris1$Sepal.Length, clusters$cluster==3), subset(iris1$Sepal.Width, clusters$cluster==3) , col="green")
points(clusters$centers[,1],clusters$centers[,2], cex=3, pch=8 )
points(head(iris_clustered[order(-distances),],n=5)$Sepal.Length,head(iris_clustered[order(-distances),],n=5)$Sepal.Width, cex=2, pch=3 )



