//http://mbostock.github.com/d3/ex/miserables.json
function output_flow_table(d) {
  d3.select("#flow_table")
    .html("<tr/>")
    .html("<tr><td>"+d.size+"</td></tr>");
}

// if user is running mozilla then use it's built-in WebSocketi

function find(link, links) {
  ret = [];
  for (i = 0; i < links.length; i++) {
  if( ((link.target.name == links[i].target) && 
        (link.source.name == links[i].source)) || 
      ((link.target.name == links[i].source) &&
       (link.source.name == links[i].target))) {
         ret.push(links[i]);
       }
  }
  return ret;
}

function update_link(link) {
  tag = "#" + link.target + "-" +link.source;
  if (link.source < link.target)
    tag = "#" + link.source + "-" + link.target;
  if(link.value*1000 < 1) 
    d3.select(tag)
      .style("stroke-width", (1));
  else
  d3.select(tag)
    .style("stroke-width", (link.value*10));
}

function update_table(d) {

  d3.select("#flow_table").html("");
  d3.select("#flow_table").attr("display", d.name);
  d3.select("#flow_table")
    .append("tr")
    .append("td")
    .style("text-align", "center")
    .style("font-weight", "bold")
    .text("node " + d.name);

  d3.select("#flow_table")
    .append("tr")
    .append("td")
    .style("text-align", "center")
    .style("font-weight", "bold")
    .text("network interfaces");

  for ( i = 0; i < d.dev.length; i++ ) {
    d3.select("#flow_table")
      .append("tr")
      .append("td").text("dev : " + d.dev[i].dev_id + 
          ",ip : "+d.dev[i].ip);
  }
  d3.select("#flow_table")
    .append("tr")
    .append("td")
    .style("text-align", "center")
    .style("font-weight", "bold")
    .text("flow table");
  for ( i = 0; i < d.flows.length; i++ ) {
    d3.select("#flow_table")
      .append("tr")
      .append("td").text(d.flows[i].flow + "->" + d.flows[i].action);

    d3.select("#flow_table").style("width", "100%");
  }

}

window.WebSocket = window.WebSocket || window.MozWebSocket;

var connection = new WebSocket('ws://sdnsim:1337');
var name_to_index = [];
connection.onmessage = 
function(m) {
  data = JSON.parse(m.data);
  d3.select("#timer").text(data.ts);
  if(data.type == "topology") {
    topo = JSON.parse(data.data);

    // local storage names to ix in order to use
    // on link updtes
    for(i=0; i<topo.nodes.length;i++) {
      name_to_index[topo.nodes[i].name] = i;
    }
    replot(topo);
  } else if (data.type == "link_utilization") {
    stats = JSON.parse(data.data);
    links = []; 
    for (i = 0; i <stats.length; i++) {
      link = {};
      link.source = stats[i].source;
      link.target = stats[i].target;
      link.ts = stats[i].ts;
      link.value = stats[i].value;
      links.push(link);
    }
    var link = svg.selectAll(".link")
      .each(
          function(d){
            data = find(d, links);
            for (i = 0; i < data.length; i++) {
              link = data[i];
              if ((d.ts == link.ts) && 
                (d.value < link.value)) {
                  d.value = link.value;
                  update_link(link);
                } else if (d.ts < link.ts) {
                  d.ts = link.ts;
                  d.value = link.value;
                  update_link(link);
                }
            }
          });
  } else if (data.type == "node_dev") {
    dev = JSON.parse(data.data);
    nodes = svg.selectAll(".node")
      .each (function (d) {
        if ( d.name == dev.name) {
          d.dev.push({"dev_id":dev.dev_id,"ip":dev.ip});
//          alert("adding dev: " + dev.dev_id + " ip:" + dev.ip + " on " + 
//            dev.name + ",devices:" + String(d.dev.length));
        }

      });
  } else if (data.type == "flow") {
    flow = JSON.parse(data.data);
    nodes = svg.selectAll(".node")
      .each (function (d) {
        if ( d.name == flow.name) {
          if (flow.type == "del") {
            for (i=0;i<d.flows.length;i++) 
        if(d.flows[i].flow == flow.flow) 
        d.flows.splice(i,1);
          } else if (flow.type == "add") {
            add = true;
            for (i=0;i<d.flows.length;i++) {
              if(d.flows[i].flow == flow.flow) {
                add = false;
                break;
              }
            }
            if (add) 
              d.flows.push({"flow":flow.flow,"action":flow.action});
          }
          if (d3.select("#flow_table").attr("display") == flow.name) {
            update_table(d);
          }
        }
      });
  }
}; 

var width = "1152px",
    height = "800px";

var color = d3.scale.category20();
// var force = d3.layout.force()
//   //  .charge(-120)
//   // .linkDistance(30)
//   // .size([width, height])
//   ;

var force = d3.layout.force().gravity(1).linkDistance(50).charge(-3000);
 
  var svg = d3.select("#chart").append("svg")
  .attr("width", width)
  .attr("height", height);

  function replot(json) {
    svg.selectAll(".link").remove();
    svg.selectAll(".node").remove();

    // $.each (json.links, (function (d) {
    //   d.x=0; d.y=0;d.x1=0; d.y1=0;d.x2=0; d.y2=0;}));
    force
      .nodes(json.nodes)
      .charge(-100)
      .links(json.links)
      .gravity(0)
      .linkDistance(100)
      .linkStrength(8)
      .charge(-100)
      .start();

    // force
    //   .nodes(json.nodes).charge(-100)
    //   .links(json.links)
    //   .gravity(0)
    //   .linkDistance(100)
    //   .linkStrength(8)
    //   .charge(-100)
    //   .start();
    
    var link = svg.selectAll(".link")
      .data(json.links)
      .enter().append("line")
      .attr("class", "link")
      .attr("id", function(d) {
        tag = d.target.name + "-" +d.source.name;
        if (d.source.name < d.target.name)
        tag = d.source.name+"-"+d.target.name;
      return tag
      })
    .style("stroke-width", function(d) {
      if(d.value * 100 < 1)
      return 1;
      else 
      return d.value*10;
    });

    var node = svg.selectAll(".node")
      .data(json.nodes)
      .enter().append("g")
      .attr("class", "node")
      .call(force.drag)
      .on('click', function(d) { 
        update_table(d);
      });



    node.append("circle")
      .attr("r", 10)
      .style("fill", "blue");

    node.append("text")
      .style("stroke", "black")
      .text(function(d) { return d.name });

    force.on("tick", function() {
      link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });
    node.attr("transform", 
      function(d) { 
        return "translate(" + d.x + "," + d.y + ")"; 
      });
    }); 
  }

