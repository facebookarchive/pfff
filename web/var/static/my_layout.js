
// Nested layouts for d3, inspired by http://moose.unibe.ch/tools/mondrian

var my = {};

my.layout = function() {

    var children = function(self) { return self },
        child_layout = null,
        fun = [],
        margin = 0,
        min_height = 0,
        min_width = 0,
        padding = 0;

    // Helper functions

    function my_width(self) {
        var w = d3.max(children(self), function(d) { return d.x + d.w });
        return Math.max(w || 0, min_width);
    }

    function my_height(self) {
        var h = d3.max(children(self), function(d) { return d.y + d.h });
        return Math.max(h || 0, min_height);
    }

    // The main working horse

    var layout = function(self) {
        children(self).forEach(function(d) { child_layout && child_layout(d) });
        fun.forEach(function(f) { f(self) });
        self.w = my_width(self) + 2 * margin;
        self.h = my_height(self) + 2 * margin;
    }

    // Descent down to the next nesting level

    layout.group = function() {
        return child_layout = my.layout();
    }

    // Setters and getters

    layout.nodes = function(value) {
        children = value;
        return layout;
    }

    layout.margin = function(value) {
        margin = value;
        return layout;
    }

    layout.minHeight = function(value) {
        min_height = value;
        return layout;
    }

    layout.minWidth = function(value) {
        min_width = value;
        return layout;
    }

    layout.padding = function(value) {
        padding = value;
        return layout;
    }

    // Arrangement

    layout.besideEachOther =
    layout.horizontally = function(value) {
        fun.push(function(self) {
            var x = margin;
            children(self).forEach(function(d) {
                d.x = x;
                x += d.w + padding;
            })
        })
        return layout;
    }

    layout.belowEachOther =
    layout.vertically = function(value) {
        fun.push(function(self) {
            var y = margin;
            children(self).forEach(function(d) {
                d.y = y;
                y += d.h + padding;
            })
        })
        return layout;
    }

    // Alignment

    function alignment(aggregation, callback) {
        return function() {
            fun.push(function(self) {
                var a = aggregation && aggregation(self);
                children(self).forEach(function(d) {
                    callback(d, a)
                })
            });
            return layout;
        }
    }

    layout.alignRight = alignment(
        undefined, function(d) { d.x = margin });
    layout.alignCenter = alignment(
        my_width, function(d,w) { d.x = (w - d.w) / 2 + margin});
    layout.alignLeft = alignment(
        my_width, function(d,w) { d.x = w - d.w + margin });

    layout.alignTop = alignment(
        undefined, function(d) { d.y = margin });
    layout.alignMiddle = alignment(
        my_height, function(d,h) { d.y = (h - d.h) / 2 + margin });
    layout.alignBottom = alignment(
        my_height, function(d,h) { d.y = h - d.h + margin });

    // Common function use in d3 selections

    layout.transform = function(d) { return 'translate('+d.x+','+d.y+')' }
    layout.w = function(d) { return d.w }
    layout.h = function(d) { return d.h }


    // Start with all children aligned to top right.

    return layout
        .alignTop()
        .alignRight();

}

my.layout.example = function() {

    var w = function() { return 10 + Math.random() * 40 }
    var h = function() { return 15 + Math.random() * 20 }

    var data = d3.nest()
        .key(function(d) { return Math.floor(Math.random() * 7) })
        .entries(d3.range(40).map(function() { return { w: w(), h: h() }}));

    var layout = my.layout();

    layout
        .margin(4)
        .padding(12)
        .belowEachOther()
        .alignCenter()
      .group()
        .padding(4)
        .besideEachOther()
        .alignBottom()
        .nodes(function(d) { return d.values });

    layout(data);

    d3.select('body')
        .append('svg')
        .attr('width', data.w)
        .attr('height', data.h)
        .selectAll('g')
        .data(data)
      .enter()
        .append('g')
        .attr('transform', layout.transform)
        .selectAll('g')
        .data(function(d) { return d.values })
      .enter()
        .append('g')
        .attr('transform', layout.transform)
        .append('rect')
        .attr('stroke', 'blue')
        .attr('fill', '#eee')
        .attr('width', layout.w)
        .attr('height', layout.h);

}
