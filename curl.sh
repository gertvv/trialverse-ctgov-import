# remember to define $APIKEY and $DATASET
. secrets.sh

lein run $1.xml >$1.ttl || exit
curl -s -D headers.txt -X PUT "$DATASET/graphs/$1?commitTitle=Import+from+ClinicalTrials.gov" -H "X-Auth-Application-Key: $APIKEY" -H "Content-Type: text/turtle" --data-binary "@$1.ttl"

cat headers.txt
