"use strict";
$(function () {
    var APIConnection = function(pathname) {
        var wsUrl = (window.location.protocol === "https:" ? "wss://" : "ws://") + window.location.host + pathname;
        var connAttempts = 0;
        var conn = null;
        var callback = null;
        var retry = function () {
            if (++connAttempts < 10) {
                conn = new window.WebSocket(wsUrl);
                conn.onmessage = callback;
                conn.onerror = function() {
                    console.log("WS connection errored");
                    retry();
                };
                conn.onclose = function() {
                    console.log("WS connection closed");
                };
            }
        };
        retry();
        return {
            registerCallback: function(func) {
                conn.onmessage = callback = func;
            },
            readyState: function () {
                return conn ? conn.readyState : -1;
            },
            close : function () {
                conn.close();
            }
        };
    };

    var EntityRow = React.createClass({displayName: 'EntityRow',
        render: function() {
            if (this.props.firstRowSpan)
                return (React.createElement("tr", null, React.createElement("th", {rowSpan: this.props.firstRowSpan}, this.props.entity.category), 
                            React.createElement("td", null, this.props.entity.name)
                        ));
            else
                return (React.createElement("tr", null, React.createElement("td", null, this.props.entity.name)));
        }
    });

    var EntityGroup = React.createClass({displayName: 'EntityGroup',
        render: function() {
            var length = this.props.entities.length;
            var rows = _.map(this.props.entities, function(entity, i) {
                return (React.createElement(EntityRow, {key: entity.id, entity: entity, firstRowSpan: i ? 0 : length}));
            });
            return (React.createElement("tbody", null, rows));
        }
    });

    var EntityTable = React.createClass({displayName: 'EntityTable',
        getInitialState: function() {
            return {entities: {}};
        },
        componentDidMount: function() {
            var that = this;
            this.props.conn.registerCallback(function(e) {
                var groupedData = _.groupBy(JSON.parse(e.data).data, 'category');
                var groupedDataSorted = _.object(_.map(groupedData, function(v, k) {
                    return [k, _.sortBy(v, 'name')];
                }));
                that.setState({entities: groupedDataSorted});
            });
        },
        componentWillUnmount: function () {
            this.props.conn.close();
        },
        render: function() {
            var idx = 0;
            var rows = _.map(this.state.entities, function(entities) {
                return (React.createElement(EntityGroup, {entities: entities, key: idx++}));
            });
            return (React.createElement("div", {className: "table-responsive"}, React.createElement("table", {className: "table"}, React.createElement("thead", null, React.createElement("tr", null, React.createElement("th", null, "Category"), React.createElement("th", null, "Name"))), rows)));
        }
    });

    var BSTab = React.createClass({displayName: 'BSTab',
        render: function() {
            return (React.createElement("li", {role: "presentation", className: this.props.active ? "active" : ""}, React.createElement("a", {'data-target': this.props.target, 'aria-controls': "home", role: "tab", 'data-toggle': "tab"}, this.props.label)));
        },
        componentDidMount: function() {
            if ("willShow" in this.props) {
                $(this.getDOMNode()).on("show.bs.tab", this.props.willShow);
            }
            if ("didHide" in this.props) {
                $(this.getDOMNode()).on("hidden.bs.tab", this.props.didHide);
            }
        }
    });

    var BSTabContent = React.createClass({displayName: 'BSTabContent',
        render: function() {
            return (React.createElement("div", {role: "tabpanel", className: this.props.active ? "tab-pane fade in active" : "tab-pane fade in", id: this.props.name}, this.props.children));
        }
    });

    var BSModal = React.createClass({displayName: 'BSModal',
        render: function() {
            var header = this.props.canClose ? (React.createElement("div", {className: "modal-header"}, React.createElement("button", {type: "button", className: "close", 'data-dismiss': "modal"}, React.createElement("span", {'aria-hidden': "true"}, "×"), React.createElement("span", {className: "sr-only"}, "Close")), React.createElement("h4", {className: "modal-title"}, this.props.title))) : (React.createElement("div", {className: "modal-header"}, React.createElement("h4", {className: "modal-title"}, this.props.title)));
            var footer = this.props.buttons ? (React.createElement("div", {className: "modal-footer"}, this.props.buttons)) : (React.createElement("div", null));
            return (React.createElement("div", {id: "modal", className: "modal fade"}, React.createElement("div", {className: "modal-dialog"}, React.createElement("div", {className: "modal-content"}, 
                    header, 
                    React.createElement("div", {className: "modal-body"}, this.props.children), 
                    footer
                    ))));
        },
        componentDidMount: function() {
            $(this.getDOMNode()).modal({keyboard: false, backdrop: 'static'});
        }
    });

    var EntityTableTab = React.createClass({displayName: 'EntityTableTab',
        render: function() {
            var name = this.props.name;
            var willShow = function () {
                React.render(React.createElement(EntityTable, {conn: APIConnection("/api/" + name + "s")}), $("#" + name).get(0));
            };
            var didHide = function () {
                React.unmountComponentAtNode($("#" + name).get(0));
            };
            return (React.createElement(BSTab, {active: 0, target: "#" + name, label: this.props.label, willShow: willShow, didHide: didHide}));
        }
    });

    var Page = React.createClass({displayName: 'Page',
        render: function() {
            return (React.createElement("div", {className: "container"}, 
                    React.createElement("div", {className: "page-header"}, 
                    React.createElement("h1", null, "RVHS Science Lab Undertaking — Admin Console")
                    ), 
                    React.createElement("p", null, "You are logged in as xxx."), 
                      React.createElement("div", {role: "tabpanel"}, 
                        React.createElement("ul", {className: "nav nav-tabs", role: "tablist"}, 
                          React.createElement(BSTab, {active: 1, target: "#home", label: "Home"}), 
                          React.createElement(EntityTableTab, {name: "cca", label: "Manage CCAs"})
                        ), 
                        React.createElement("div", {className: "tab-content"}, 
                          React.createElement(BSTabContent, {active: 1, name: "home"}, React.createElement("p", null, "ehh")), 
                          React.createElement(BSTabContent, {active: 0, name: "cca"})
                        )
                    )
                    ));
        }
    });

    $("body").append("<div id='modal-wrapper'/>").append("<div id='wrapper'/>");
    if (undefined === window.WebSocket) {
        React.render(React.createElement(BSModal, {canClose: 0, title: "Browser Unsupported"}, React.createElement("p", null, "Your browser is too old to use this website. This website requires ", React.createElement("strong", null, "at least"), " Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available.")), $("#modal-wrapper").get(0));
        return false;
    }
    React.render(React.createElement(Page, null), $("#wrapper").get(0));

    return true;
});
