let run () =
  let _ = OS.Time.set_duration 10 in 
  let _ = OS.Topology.add_node "host2" (Simple_tcp_test.host_inner  2) in
  let _ = OS.Topology.add_node "host1" (Simple_tcp_test.host_inner  1) in
  let _ = OS.Topology.add_link ~prop_delay:10 ~rate:10 ~pcap:true "host1"
  "host2" in	
    ()

let _ = OS.Topology.load run 
