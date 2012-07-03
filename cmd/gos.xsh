#!xsh -al
#$tmp := open --file "../template.xml";
#$wrapper = $tmp/html/body/div#wrapper;
#copy $content into $wrapper;
#save --file $ARGV[2] $wrapper;

$doc := open --file $ARGV[1];
$content = $doc/html/body/div[2];
save --file $ARGV[2] $content;
