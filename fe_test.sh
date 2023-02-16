echo -e "Tests:\n"
allpass=1
passcount=0
failcount=0
validcount=221
invalidcount=133

shopt -s globstar
for file in src/test/scala/wacc/front_end/**/*.wacc
do 
    ((testcount=testcount+1))
    expected=$(grep -A1 "# Output" $file)
    if echo $expected | grep -q "# #syntax_error"
        then
        expected_exit=100
    elif echo $expected | grep -q "# #semantic_error"
        then
        expected_exit=200
    else
        expected_exit=0
    fi

    output=$(./compile $file)
    exit=$?
    pass=1

    # Check exit code
    if [ $exit -eq $expected_exit ]
        then
        echo -e "Test $file passed"
        ((passcount=passcount+1))
    else
        echo -e "Test $file failed! Exited $exit instead of $expected_exit"
        ((failcount=failcount+1))
        allpass=0
    fi
done

echo -e -n "Current progress: "
echo "scale=2;100*$passcount/($validcount+$invalidcount)" | bc | tr '\n' ' '
echo -e "% of tests in wacc_example passed"

if [ $allpass -eq 1 ]
    then 
    echo -e "All $passcount tests passed"
    exit 0
else
    echo -e "$failcount tests failed! $passcount tests passed"
    exit 1
fi