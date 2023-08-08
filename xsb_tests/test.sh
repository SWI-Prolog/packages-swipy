
#../../../bin/xsb -e "[janus],init_janus:test_janus,halt. " >& /dev/null
../../../bin/xsb -e "[janus],init_janus:test_janus,halt. "

grep '!!!' test_janus_out_new

status=0
diff -w  test_janus_out_new test_janus_out_old || status=1
if test "$status" = 0 ; then 
	echo "janus successfuly tested"
	rm -f test_janus_out_new
else
	echo "janus test failed: test_janus_outs differ!!!"
	diff -w  test_janus_out_new test_janus_out_old
fi

