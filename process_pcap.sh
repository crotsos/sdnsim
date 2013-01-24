 #!/usr/bin/env bash
#
   
echo "looking for pcap files in $1"

rm tmp.pcap

for file in $1/ns3-*.pcap; do
  echo "process file $file";
  bittwiste -I $file -O tmp.pcap -M 1 -D 1-2;
  mv tmp.pcap $file;
  #  editcap -s 90 $file tmp.pcap
  #  mv tmp.pcap $file
done
