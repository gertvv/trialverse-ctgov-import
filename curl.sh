. secrets.sh

DATASET=https://tv-test.drugis.org/users/e17ae0bd2a6888426411e53fb7a1825aca3fc02b1acb6de4d3cead1ff36adab6/datasets/77f6bd2e-26ba-43f5-911e-66710bd6906b

lein run $1.xml >$1.ttl || exit
curl -s -D headers.txt -X PUT "$DATASET/graphs/$1?commitTitle=Import+from+ClinicalTrials.gov" -H "X-Auth-Application-Key: $APIKEY" -H "Content-Type: text/turtle" --data-binary "@$1.ttl"

cat headers.txt
