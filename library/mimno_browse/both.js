var dateFactorTopics;
var topicLabels;
var wordFrequencies;
var topicDocs;
var docs;

function ready(error, dateFactorTopics_, topicLabels_, wordFrequencies_, topicDocs_, docs_) {

  dateFactorTopics = dateFactorTopics_;
  topicLabels = topicLabels_;
  wordFrequencies = wordFrequencies_;
  topicDocs = topicDocs_;
  docs = docs_;

  var maxTopic = d3.max(topicDocs, function(d) {return d.topic;});
  var justFactors = dateFactorTopics.filter(function (d) {return d.factor != "other";} );
  var factorLabel = d3.max(justFactors, function(d) {return d.factor;});

  topicLabels.forEach(function(topicLabel, topicIndex) {

    var selectedTopic = topicLabel.topic;
    var topicDiv = d3.select("#topics").append("div").attr("class", "topic");

    var padding = 50;

    var topRow = topicDiv.append("div");
    var expandButtonRow = topicDiv.append("div").attr("class", "expandbutton");
    var bottomRow = topicDiv.append("div").attr("class", "details");

    expandButtonRow.append("button").text("more/less information").on("click", function () {
      if (bottomRow.style("display") == "none") { bottomRow.style("display", "block"); }
      else { bottomRow.style("display", "none"); }
    });

    var timePlot = topRow.append("svg").attr("height", 200).attr("width", 600).attr("class", "timeseries");
    var timeGroup = timePlot.append("g")
      .attr("transform", "translate(" + padding + "," + padding + ")");

    var factorValues = dateFactorTopics.filter(function (d) { 
      return d.factor != "other" && d.topicLabel == topicLabel.label; } );
    var otherValues = dateFactorTopics.filter(function (d) { 
      return d.factor == "other" && d.topicLabel == topicLabel.label; } );

    var maxY = d3.max( [d3.max(factorValues, 
      function (d) { return d.high; }), d3.max(otherValues, function (d) { return d.high; })] );

    var dates = factorValues.map(function (d) { return d.date; });

    var xScale = d3.scale.ordinal().domain(["7/29/2003", "7/30/2003", "7/31/2003", "8/1/2003", "10/26/2004", "10/27/2004", "10/28/2004"]).rangePoints([padding, 600 - 5], 0.5);
    var yScale = d3.scale.linear().domain([0, maxY]).range([200 - padding, 5]);

    	var xAxis = d3.svg.axis().scale(xScale);
    	timePlot.append("g")
    	        .attr("class", "axis")
    	        .attr("transform", "translate(0, " + (200 - padding) + ")")
    	        .call(xAxis);
    	
    	var yAxis = d3.svg.axis().scale(yScale).orient("left");
    	timePlot.append("g")
    	        .attr("class", "axis")
    	        .attr("transform", "translate(" + padding + ", 0)")
    	        .call(yAxis);
    	
    	var line = d3.svg.line()
    	  .x(function (day) {return xScale(day.date);})
              .y(function (day) { return yScale(day.mean); });
    		
    	var area = d3.svg.area()
    	  .x(function (day) {return xScale(day.date);})
              .y(function (day) { return yScale(day.high); })
              .y0(function (day) { return yScale(day.low); });
    		
    	timePlot.append("text").style("fill", "blue").attr("x", 450).attr("y", 15).text(factorLabel);
    	timePlot.append("text").style("fill", "red").attr("x", 550).attr("y", 15).text("Others");

    	timePlot.append("path").attr("d", area(factorValues.filter(function (d) { return d.date.substr(0, 1) == "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "blue").style("opacity", "0.1");

    	timePlot.append("path").attr("d", line(factorValues.filter(function (d) { return d.date.substr(0, 1) == "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "none").style("stroke", "blue");

    	timePlot.append("path").attr("d", area(otherValues.filter(function (d) { return d.date.substr(0, 1) == "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "red").style("opacity", "0.1");

    	timePlot.append("path").attr("d", line(otherValues.filter(function (d) { return d.date.substr(0, 1) == "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "none").style("stroke", "red");

    	timePlot.append("path").attr("d", area(factorValues.filter(function (d) { return d.date.substr(0, 1) != "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "blue").style("opacity", "0.1");

    	timePlot.append("path").attr("d", line(factorValues.filter(function (d) { return d.date.substr(0, 1) != "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "none").style("stroke", "blue");

    	timePlot.append("path").attr("d", area(otherValues.filter(function (d) { return d.date.substr(0, 1) != "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "red").style("opacity", "0.1");

    	timePlot.append("path").attr("d", line(otherValues.filter(function (d) { return d.date.substr(0, 1) != "1"; })))
    	  .attr("class", "line")
    	  .style("fill", "none").style("stroke", "red");

    topRow.append("span").text((topicIndex + 1) + ". ");
    topRow.append("span").attr("class", "topwords").text(topicLabel.words);

    topicDiv.append("div").attr("class", "clearing");

    var selectedTopicWords = wordFrequencies.filter(function (d) { return d.topic == selectedTopic; });

    var realWords = selectedTopicWords.filter(function (word) { return word.type == "real"; });
    var replicatedWords = selectedTopicWords.filter(function (word) { return word.type == "replicated"; });
    var selectedTopicDocs = topicDocs.filter(function (d) { return d.topic == selectedTopic; });


    padding = 30;

    var areaHeight = 400;
    var areaWidth = 100;

    var h = areaHeight + 3 * padding;
    var w = 2 * areaWidth + 4 * padding;

    var chart = bottomRow.append("div").attr("class", "chart")
      .append("svg")
      .attr("width", w)
      .attr("height", h);

    // Factor/Other plot

    var vis = chart.append("g")
      .attr("transform", "translate(" + padding + "," + (2 * padding) + ")");

    var logExtent = d3.extent( selectedTopicWords, function(word) { return Math.log( word.role[0] / word.role[1] ); } );
    var xScale = d3.scale.linear().domain(logExtent).range([0, areaWidth]);
    var yScale = d3.scale.ordinal().domain(realWords.sort(function (a, b) { return (a.role[0]/a.role[1]) - (b.role[0]/b.role[1]); }).map(function (word) { return word.word; })).rangePoints([0, areaHeight]);

    var percentFormat = d3.format(".1f");

    var xAxis = d3.svg.axis()
      .scale(xScale)
      .orient("bottom")
      .ticks(2)
      .tickFormat(d3.format("d"))
      .tickValues([1, 2]);

    var fakePoints = vis.selectAll(".repdot").data(replicatedWords).enter()
      .append("circle")
      .attr("cx", function(d) { return xScale( Math.log(d.role[0] / d.role[1]) ); } )
      .attr("cy", function(d) {return yScale(d.word);})
      .attr("r", 4)
      .attr("class", "repdot")
      .style("fill", "#999").style("opacity", 0.3);

    var realPoints = vis.selectAll(".realdot").data(realWords).enter()
      .append("circle")
      .attr("cx", function(d) { return xScale( Math.log(d.role[0] / d.role[1]) ); } )
      .attr("cy", function(d) {return yScale(d.word);})
      .attr("r", 4)
      .attr("class", "realdot")
      .style("fill", "#333");

    var words = vis.selectAll(".words").data(realWords).enter()
      .append("text")
      .attr("x", xScale(0))
      .attr("y", function(d) {return yScale(d.word) - 15;})
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .text(function(d) { return d.word; });

    vis.append("text")
      .attr("x", padding)
      .attr("y", -30)
      .attr("text-anchor", "end")
      .attr("text-decoration", "underline")
      .text("Values");

    vis.append("text")
      .attr("x", areaWidth)
      .attr("y", -30)
      .attr("text-anchor", "end")
      .attr("text-decoration", "underline")
      .text("World");

    // World/Values plot

    vis = chart.append("g")
      .attr("transform", "translate(" + (areaWidth + 3 * padding) + "," + (2 * padding) + ")");

    logExtent = d3.extent( selectedTopicWords, function(word) { return Math.log( word.jam[0] / word.jam[1] ); } );
    xScale = d3.scale.linear().domain(logExtent).range([0, areaWidth]);
    yScale = d3.scale.ordinal().domain(realWords.sort(function (a, b) { return (a.jam[0]/a.jam[1]) - (b.jam[0]/b.jam[1]); }).map(function (word) { return word.word; })).rangePoints([0, areaHeight]);

    xAxis = d3.svg.axis()
      .scale(xScale)
      .orient("bottom")
      .ticks(2)
      .tickFormat(d3.format("d"))
      .tickValues([1, 2]);

    fakePoints = vis.selectAll(".repdot").data(replicatedWords).enter()
      .append("circle")
      .attr("cx", function(d) { return xScale( Math.log(d.jam[0] / d.jam[1]) ); } )
      .attr("cy", function(d) {return yScale(d.word);})
      .attr("r", 4)
      .attr("class", "repdot")
      .style("fill", "#999").style("opacity", 0.3);

    realPoints = vis.selectAll(".realdot").data(realWords).enter()
      .append("circle")
      .attr("cx", function(d) { return xScale( Math.log(d.jam[0] / d.jam[1]) ); } )
      .attr("cy", function(d) {return yScale(d.word);})
      .attr("r", 4)
      .attr("class", "realdot")
      .style("fill", "#333");

    words = vis.selectAll(".words").data(realWords).enter()
      .append("text")
      .attr("x", xScale(0))
      .attr("y", function(d) {return yScale(d.word) - 15;})
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .text(function(d) { return d.word; });

    vis.append("text")
      .attr("x", padding)
      .attr("y", -30)
      .attr("text-anchor", "end")
      .attr("text-decoration", "underline")
      .text(factorLabel);

    vis.append("text")
      .attr("x", areaWidth)
      .attr("y", -30)
      .attr("text-anchor", "end")
      .attr("text-decoration", "underline")
      .text("Other");

    // Topic top docs

    bottomRow.append("div").attr("class", "documents").append("div").attr("class", "docbox").selectAll(".document").data(selectedTopicDocs).enter()
      .append("div")
      .attr("class", "document")
      .text(function (d) { 
        var doc = docs[ d.doc ];
        return doc.id + ", " + doc.CreationDate + " [" + doc.factor + "]: " + doc.text;
      });

    topicDiv.append("div").attr("class", "clearing");
  });
}
