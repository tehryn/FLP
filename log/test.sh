RED='\033[0;31m'
GREEN='\033[0;32m'
YEL='\033[0;33m'
NC='\033[0m'
total=0
ok=0
for file in testy/*.in
do
    name=$(basename "${file}")
    num=${name%.in}
    ./flp19-log < $file >testy/${num}.tmp
    diff testy/${num}.tmp testy/${num}.out
    if [ "$?" == "0" ]
    then
        echo -e "${GREEN}[TEST $total] ./flp19-log < $file ${NC}"
        ((ok++))
    else
        echo -e "${RED}[TEST $total] ./flp19-log < $file ${NC}"
    fi
    ((total++))
done

if [ "$ok" == "$total" ]
then
    echo -e "[${GREEN}SUCCESSFULL${NC}]"
else
    echo -e "[${RED}FAILED${NC}]"
fi

rm testy/*.tmp