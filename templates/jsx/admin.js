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

    var EntityRow = React.createClass({
        render: function() {
            if (this.props.firstRowSpan)
                return (<tr><th rowSpan={this.props.firstRowSpan}>{this.props.entity.category}</th>
                            <td>{this.props.entity.name}</td>
                        </tr>);
            else
                return (<tr><td>{this.props.entity.name}</td></tr>);
        }
    });

    var EntityGroup = React.createClass({
        render: function() {
            var length = this.props.entities.length;
            var rows = _.map(this.props.entities, function(entity, i) {
                return (<EntityRow key={entity.id} entity={entity} firstRowSpan={i ? 0 : length} />);
            });
            return (<tbody>{rows}</tbody>);
        }
    });

    var EntityTable = React.createClass({
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
                return (<EntityGroup entities={entities} key={idx++} />);
            });
            return (<div className="table-responsive"><table className="table"><thead><tr><th>Category</th><th>Name</th></tr></thead>{rows}</table></div>);
        }
    });

    var BSTab = React.createClass({
        render: function() {
            return (<li role="presentation" className={this.props.active ? "active" : ""}><a data-target={this.props.target} aria-controls="home" role="tab" data-toggle="tab">{this.props.label}</a></li>);
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

    var BSTabContent = React.createClass({
        render: function() {
            return (<div role="tabpanel" className={this.props.active ? "tab-pane fade in active" : "tab-pane fade in"} id={this.props.name}>{this.props.children}</div>);
        }
    });

    var BSModal = React.createClass({
        render: function() {
            var header = this.props.canClose ? (<div className="modal-header"><button type="button" className="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span className="sr-only">Close</span></button><h4 className="modal-title">{this.props.title}</h4></div>) : (<div className="modal-header"><h4 className="modal-title">{this.props.title}</h4></div>);
            var footer = this.props.buttons ? (<div className="modal-footer">{this.props.buttons}</div>) : (<div />);
            return (<div id="modal" className="modal fade"><div className="modal-dialog"><div className="modal-content">
                    {header}
                    <div className="modal-body">{this.props.children}</div>
                    {footer}
                    </div></div></div>);
        },
        componentDidMount: function() {
            $(this.getDOMNode()).modal({keyboard: false, backdrop: 'static'});
        }
    });

    var EntityTableTab = React.createClass({
        render: function() {
            var name = this.props.name;
            var willShow = function () {
                React.render(<EntityTable conn={APIConnection("/api/" + name + "s")} />, $("#" + name).get(0));
            };
            var didHide = function () {
                React.unmountComponentAtNode($("#" + name).get(0));
            };
            return (<BSTab active={0} target={"#" + name} label={this.props.label} willShow={willShow} didHide={didHide} />);
        }
    });

    var Page = React.createClass({
        render: function() {
            return (<div className="container">
                    <div className="page-header">
                    <h1>RVHS Science Lab Undertaking &mdash; Admin Console</h1>
                    </div>
                    <p>You are logged in as xxx.</p>
                      <div role="tabpanel">
                        <ul className="nav nav-tabs" role="tablist">
                          <BSTab active={1} target="#home" label="Home" />
                          <EntityTableTab name="cca" label="Manage CCAs" />
                        </ul>
                        <div className="tab-content">
                          <BSTabContent active={1} name="home"><p>ehh</p></BSTabContent>
                          <BSTabContent active={0} name="cca" />
                        </div>
                    </div>
                    </div>);
        }
    });

    $("body").append("<div id='modal-wrapper'/>").append("<div id='wrapper'/>");
    if (undefined === window.WebSocket) {
        React.render(<BSModal canClose={0} title="Browser Unsupported"><p>Your browser is too old to use this website. This website requires <strong>at least</strong> Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available.</p></BSModal>, $("#modal-wrapper").get(0));
        return false;
    }
    React.render(<Page />, $("#wrapper").get(0));

    return true;
});
