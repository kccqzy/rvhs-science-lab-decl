// -*- js2-basic-offset: 4; -*-
"use strict";
((window, document, React, ReactDOM, $, _) => {
$(function() {
    // Here are a few aliases to save typing and help with minification.
    let React_createElement = React.createElement;
    let E = React_createElement;
    let React_createClass = React.createClass;
    let React_Component = React.Component;
    let React_PropTypes = React.PropTypes;
    let __map = _.map;

    // Identity of current user through meta elements
    let identUser = $("#meta-user").attr("value");
    let identIsAdmin = (() => {
        let identPriv = $("#meta-priv").attr("value");
        return identPriv === "PrivAdmin" || identPriv === "PrivOperator";
    })();

    // An API Connection using WebSocket. Using SSE is now preferred, and so
    // this is for Internet Explorer only. The APIConnectionWS and
    // APIConnectionSSE are identical in interface. Both are old-style classes
    // in which constructor is a simple function that returns an object of
    // functions.
    let APIConnectionWS = (pathname) => {
        let wsUrl = (window.location.protocol === "https:" ? "wss://" : "ws://") + window.location.host + pathname;
        let conn = null;
        let callback = null;
        let timeConnected = null;

        let connect = () => {
            if (Date.now() - timeConnected > 2000) {
                timeConnected = Date.now();
                conn = new window.WebSocket(wsUrl);
                conn.onmessage = callback;
                conn.onerror = () => {
                    console.log("APIConnectionWS: conn.onerror");
                    connect();
                };
                conn.onclose = (e) => {
                    console.log("APIConnectionWS: conn.onclose");
                    console.log(e);
                    connect();
                };
            } else {
                window.setTimeout(connect,(Date.now() - timeConnected));
            }
        };

        let close = () => {
            conn.onmessage = () => {};
            conn.onerror = () => {};
            conn.onclose = () => {};
            callback = null;
            conn.close();
        };

        return {
            registerCallback: (func) => {
                if (!conn) connect();
                let wrapFunc = (e) => func(JSON.parse(e.data));
                conn.onmessage = wrapFunc;
                callback = wrapFunc;
            },
            pathname: () => pathname,
            close
        };
    };

    // An API Connection using server-sent events. Using SSE is now preferred,
    // and so the above WS version is deprecated, but needed for IE support.
    // Both have the same interface though. The reasons why we do not use WS is
    // because (a) the haskell websocket library is not really mature, and there
    // are a ton of interoperability errors with actual browsers; (b) proxy
    // servers can wreak havoc with WebSockets running over unsecured HTTP.
    let APIConnectionSSE = (pathname) => {
        let url = '//' + window.location.host + pathname;
        let conn = null;
        let callback = null;

        let connect = () => {
            // Fortunately, reestablishing connection is automatic.
            conn = new window.EventSource(url);
            conn.onmessage = callback;
            conn.onerror = () => {console.log("APIConnectionSSE: " + pathname + ": conn.onerror");};
            conn.onclose = () => {console.log("APIConnectionSSE: " + pathname + ": conn.onclose");};
        };

        let close = () => {
            callback = null;
            conn.close();
        };

        return {
            registerCallback: (func) => {
                if (!conn) connect();
                let wrapFunc = (e) => func(JSON.parse(e.data));
                conn.onmessage = wrapFunc;
                callback = wrapFunc;
            },
            pathname: () => pathname,
            close
        };
    };

    // An API Connection using the most rudimentary of all: polling.
    let APIConnectionPoll = (pathname) => {
        let url = '//' + window.location.host + pathname;
        let callback = null;
        let timeout = null;

        let close = () => {
            if (timeout) window.clearTimeout(timeout);
            timeout = null;
        };

        let wrapFunc = () => {
            // Perform normal AJAX request and call func
            $.ajax({url, success: (data) => {
                // jQuery has called JSON.parse already.
                if (callback) callback(data);
                timeout = window.setTimeout(wrapFunc, 3000);
            }});
        };
        wrapFunc();

        return {
            registerCallback: (func) => {
                callback = func;
            },
            pathname: () => pathname,
            close
        };
    };

    let APIConnection = window.EventSource ? APIConnectionSSE : window.WebSocket ? APIConnectionWS : APIConnectionPoll;

    // An EntityRow is a row of data in the table. Each row also has two action
    // buttons. The prop firstRowSpan is a bit confusing: when zero, the first
    // row is omitted entirely, but when nonzero, it specifies the number of
    // rows to span.
    class EntityRow extends React_Component {
        constructor(props) {
            super(props);
            this.displayName = 'EntityRow';
        }
        render() {
            let dataSpec = pageSpec[window.location.pathname].dataSpec;
            let firstCell = (() => {
                if (dataSpec.categoryColumn !== null && this.props.firstRowSpan) {
                    let value = this.props.entity[dataSpec.categoryColumn[0]];
                    let mapper = dataSpec.categoryColumn[1];
                    return E("td", {rowSpan: this.props.firstRowSpan}, mapper.apply(this.props.auxiliary, [value, this.props.entity]));
                }
                else return null;
            })();
            let onEditButtonClick = () => {
                ReactDOM.render(E(this.props.entityEditor,{
                    auxiliary: this.props.auxiliary,
                    entity: this.props.entity
                }), getModalWrapper());
            };
            let onDeleteButtonClick = () => {
                return ReactDOM.render(E(DeleteConfirmation,{
                    auxiliary: this.props.auxiliary,
                    entity: this.props.entity
                }),getModalWrapper());
            };
            let editDeleteButtonsCell = E("td",{className: "text-right"},
                                          E("div",{className: "btn-group", role: "group"},
                                            E("button",{type: "button", className: "btn btn-default btn-xs", title: "Edit", onClick: onEditButtonClick},
                                              E("span",{className: "glyphicon glyphicon-pencil", "aria-hidden": "true"})),
                                            E("button",{type: "button", className: "btn btn-default btn-xs", title: "Remove", onClick: onDeleteButtonClick},
                                              E("span",{className: "glyphicon glyphicon-trash", "aria-hidden": "true"}))));

            return E("tr", {},
                     firstCell,
                     __map(dataSpec.columns,
                           (spec, idx) => {
                               let value = this.props.entity[spec[0]];
                               let mapper = spec[1];
                               return E("td", {key: idx}, mapper.apply(this.props.auxiliary, [value, this.props.entity]));
                           }),
                     identIsAdmin ? editDeleteButtonsCell : null);
        }
    }
    EntityRow.propTypes = {
        firstRowSpan: React_PropTypes.number.isRequired,
        entity: React_PropTypes.object.isRequired,
        entityEditor: React_PropTypes.any.isRequired,
        auxiliary: React_PropTypes.object
    };

    // An EntityCategory is a group of data shared under a category,
    // presented as a tbody with a single cell spanning all of them.
    class EntityCategory extends React_Component {
        constructor(props) {
            super(props);
            this.displayName = 'EntityCategory';
        }
        render() {
            return E("tbody", {},
                     __map(this.props.entities,
                           (entity,i,entities) =>
                           E(EntityRow, {key: entity.id, entityEditor: this.props.entityEditor, auxiliary: this.props.auxiliary, entity: entity,
                                         firstRowSpan: i ? 0 : entities.length})));
        }
    }
    EntityCategory.propTypes = {
        entities: React_PropTypes.array.isRequired,
        entityEditor: React_PropTypes.any.isRequired,
        auxiliary: React_PropTypes.object
    };

    // An EntityTable is the entire table for holding the data. It
    // receives events from the two action buttons on each row.
    class EntityTable extends React_Component {
        constructor(props) {
            super(props);
            this.displayName = 'EntityTable';
            this.state = {
                tableData: {data: []}
            };
        }

        registerCallback() {
            return this.props.conn.registerCallback((d) => {
                return this.setState({
                    tableData: d
                });
            });
        }

        componentDidMount() {
            return this.registerCallback();
        }

        componentDidUpdate() {
            return this.registerCallback();
        }

        componentWillUnmount() {
            return this.props.conn.close();
        }

        componentWillReceiveProps(newProps) {
            if (this.props.conn.pathname() !== newProps.conn.pathname())
                this.props.conn.close();
        }

        render() {
            let dataSpec = pageSpec[window.location.pathname].dataSpec;
            let rawData = (this.props.customFilter || _.identity)(this.state.tableData.data);

            let rows = (() => {
                if (dataSpec.categoryColumn) {
                    let categoryName = dataSpec.categoryColumn[0];
                    let sortName = dataSpec.columns[0][0];
                    let massagedData = __map(_.sortBy(__map(_.groupBy(rawData,categoryName),
                                                            (v,k) => ({k: k, v: _.sortBy(v,sortName)})),
                                                      "k"),
                                             (d) => d.v);
                    return __map(massagedData,
                                 (entities) =>
                                 E(EntityCategory,{
                                     entities: entities,
                                     entityEditor: this.props.entityEditor,
                                     auxiliary: this.props.auxiliary,
                                     key: entities[0][categoryName]
                                 }));
                } else {
                    let sortName = dataSpec.columns[0][0];
                    let massagedData = _.sortBy(rawData,sortName);
                    return E("tbody", {},
                             __map(massagedData,
                                   (entity) =>
                                   E(EntityRow,{
                                       firstRowSpan: 1,
                                       entityEditor: this.props.entityEditor,
                                       auxiliary: this.props.auxiliary,
                                       entity: entity,
                                       key: entity.id
                                   })));
                }
            })();

            let displayColumnName = (columnName) =>
                    Object.prototype.toString.call(columnName) === "[object Function]" ?
                    columnName(rawData) :
                    columnName;

            let editDeleteButtonsCell = (() => {
                let onDeleteAllClick = () => {
                    let ids = __map(rawData, "id").join(",");
                    let count = rawData.length;
                    let countDescription = count === 1 ? "1 " + dataSpec.humanNameInSentence : count + " " + dataSpec.humanNamePluralInSentence;

                    let cannotDeleteNothingDialog = E(Modal,
                                                      {
                                                          canClose: true,
                                                          title: "Nothing Currently Shown",
                                                          buttons: E("button",{
                                                              type: "button",
                                                              className: "btn btn-default",
                                                              "data-dismiss": "modal"
                                                          },"Close"),
                                                          children: E("p", {}, "Nothing is currently shown on screen.")
                                                      });
                    let confirmDeleteModal = E(AjaxFailableActionModal,
                                               {
                                                   title: "Deleting All " + dataSpec.humanNamePlural + " Currently Shown",
                                                   actionButtonLabel: "Yes, Delete All Currently Shown",
                                                   actionButtonStyle: "danger",
                                                   ajaxParam: () => ({
                                                       url: "/api/" + dataSpec.machineName,
                                                       type: "DELETE",
                                                       data: {ids}
                                                   })
                                               },
                                               E("p", {}, "Are you sure you want to delete all ", dataSpec.humanNamePluralInSentence, " currently shown on screen? This will delete ", countDescription, ". "));
                    ReactDOM.render(count ? confirmDeleteModal : cannotDeleteNothingDialog, getModalWrapper());
                };
                let deleteIcon = E("span",{className: "glyphicon glyphicon-trash", "aria-hidden": "true"});
                return E("th", {className: 'text-right'},
                         E("button",{
                             type: "button",
                             className: "btn btn-danger btn-xs",
                             onClick: onDeleteAllClick,
                             title: "Click to delete all " + dataSpec.humanNamePluralInSentence + " currently shown."
                         },deleteIcon,React_createElement("span",{
                             className: "presentation-text",
                             "data-text": " Remove These"
                         }))
                        );
            })();

            let headers = __map(((dataSpec.categoryColumn === null ?
                                  [] :
                                  [displayColumnName(dataSpec.categoryColumn[2])]))
                                .concat(__map(dataSpec.columns, (v) => displayColumnName(v[2]))),
                                (label,idx) =>
                                E("th",{key: idx},label));
            return E("div",{className: "table-responsive"},
                     E("table",{className: "table"},
                       E("thead",{},
                         E("tr",{},
                           headers,identIsAdmin ? editDeleteButtonsCell : null)),rows));
        }
    }
    EntityTable.propTypes = {
        conn: React_PropTypes.object.isRequired,
        entityEditor: React_PropTypes.any.isRequired,
        auxiliary: React_PropTypes.object,
        customFilter: React_PropTypes.func
    };

    // The Modal dialog that takes control of input and needs to be
    // dealt with before the rest of the page is functional.
    class Modal extends React_Component {
        constructor(props) {
            super(props);
            this.displayName = 'Modal';
        }

        componentDidMount() {
            $(this.refs.modal).on("hidden.bs.modal", () => {
                ReactDOM.unmountComponentAtNode(getModalWrapper());
            }).modal({keyboard: false, backdrop: "static"});
        }

        render() {
            let header = E("div", {className: "modal-header"},
                           this.props.canClose ?
                           E("button", {type: "button", className: "close", id: "modalClose", "data-dismiss": "modal"},
                             E("span", {"aria-hidden": "true"}, "×"),
                             E("span", {className: "sr-only"}, "Close")) : "",
                           E("h4", {className: "modal-title"},
                             this.props.title));
            let footer = this.props.buttons ? E("div", {className: "modal-footer"}, this.props.buttons) : "";
            return E("div", {ref: "modal", id: "modal", className: "modal fade"},
                     E("div", {className: "modal-dialog"},
                       E("div", {className: "modal-content"},
                         header,
                         E("div", {className: "modal-body"},
                           this.props.children),
                         footer)));
        }
    }
    Modal.propTypes = {
        canClose: React_PropTypes.bool.isRequired,
        title: React_PropTypes.node.isRequired,
        buttons: React_PropTypes.node,
        children: React_PropTypes.node.isRequired
    };

    // I have not refactored the code below yet! They do not use ES6!

    var getModalWrapper = function() {
        return ($("#modal-wrapper")).get(0);
    };

    var dismissModal = function() {
        return ($("#modal")).modal("hide");
    };

    // An action modal, with custom content, a Cancel button and an
    // action button.
    var ActionModal = React_createClass(_.defaults({
        propTypes: {
            actionButtonType: React_PropTypes.string,
            actionButtonStyle: React_PropTypes.string.isRequired,
            actionButtonLabel: React_PropTypes.node.isRequired,
            title: React_PropTypes.node.isRequired,
            children: React_PropTypes.node.isRequired,
            next: React_PropTypes.func.isRequired
        },
        getInitialState: function() {
            return {
                spinner: 0
            };
        },
        render: function() {
            var that = this;
            var onActionButtonClick = function(e) {
                e.preventDefault();
                var setSpinner = function(v) {
                    return that.setState({
                        spinner: v
                    });
                };
                return that.props.next(function() {
                    return dismissModal();
                },setSpinner);
            };
            var buttons = React_createElement("div",{},React_createElement("img",{
                width: 16,
                height: 16,
                src: "/static/res/loading.gif",
                style: {
                    display: (this.state.spinner ?
                        "inline" :
                        "none")
                }
            }),React_createElement("div",{
                style: {
                    display: (this.state.spinner ?
                        "none" :
                        "block")
                }
            },React_createElement("button",{
                type: "button",
                className: "btn btn-default",
                "data-dismiss": "modal"
            },"Cancel"),React_createElement("button",{
                type: (this.props.actionButtonType || "button"),
                className: ("btn btn-" + this.props.actionButtonStyle),
                onClick: onActionButtonClick
            },this.props.actionButtonLabel)));
            return React_createElement(Modal,{
                canClose: true,
                title: this.props.title,
                buttons: buttons
            },this.props.children);
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        ActionModal: "displayName"
    })));

    var AjaxFailableActionModal = React_createClass(_.defaults({
        propTypes: {
            actionButtonType: React_PropTypes.string,
            actionButtonStyle: React_PropTypes.string.isRequired,
            actionButtonLabel: React_PropTypes.node.isRequired,
            title: React_PropTypes.node.isRequired,
            children: React_PropTypes.node.isRequired,
            ajaxParam: React_PropTypes.func.isRequired
        },
        getInitialState: function() {
            return {
                err: null
            };
        },
        render: function() {
            var that = this;
            var onError = function(jqxhr) {
                var details = JSON.parse(jqxhr.responseText);
                return that.setState({
                    err: details.meta.details
                });
            };
            var next = function(hideModal,setSpinner) {
                setSpinner(1);
                var ajaxParams = _.defaults(that.props.ajaxParam.apply(that),{
                    success: hideModal,
                    error: function(jqxhr) {
                        setSpinner(0);
                        console.log("Ajax error.");
                        console.log(jqxhr);
                        return onError(jqxhr);
                    }
                });
                return $.ajax(ajaxParams);
            };
            return React_createElement(ActionModal,{
                title: this.props.title,
                actionButtonLabel: this.props.actionButtonLabel,
                actionButtonStyle: this.props.actionButtonStyle,
                actionButtonType: this.props.actionButtonType,
                next: next
            },(this.state.err ?
                React_createElement("div",{
                    className: "alert alert-danger",
                    role: "alert"
                },this.state.err) :
                null),this.props.children);
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AjaxFailableActionModal: "displayName"
    })));

    var RecordEditor = React_createClass(_.defaults({
        propTypes: {
            entityTypeHumanName: React_PropTypes.string.isRequired,
            entityTypeMachineName: React_PropTypes.string.isRequired,
            entity: React_PropTypes.object,
            children: React_PropTypes.node.isRequired
        },
        getInitialState: function() {
            return {
                err: null
            };
        },
        render: function() {
            var that = this;
            var hname = this.props.entityTypeHumanName;
            var mname = this.props.entityTypeMachineName;
            var title = (this.props.entity ?
                ("Edit " + hname) :
                ("Add a new " + hname));
            var actionButtonLabel = (this.props.entity ?
                "Edit" :
                "Add");
            var endpoint = (this.props.entity ?
                ((("/api/" + mname) + "/") + this.props.entity.id) :
                ("/api/" + mname));
            var method = (this.props.entity ?
                "PUT" :
                "POST");
            var ajaxParam = function() {
                return {
                    url: endpoint,
                    type: method,
                    data: ($("#editorForm")).serialize()
                };
            };
            return React_createElement("form",{
                id: "editorForm",
                role: "form"
            },React_createElement(AjaxFailableActionModal,{
                title: title,
                actionButtonLabel: actionButtonLabel,
                actionButtonStyle: "primary",
                actionButtonType: "submit",
                ajaxParam: ajaxParam
            },this.props.children));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        RecordEditor: "displayName"
    })));

    var CcaEditor = React_createClass(_.defaults({
        propTypes: {
            entity: React_PropTypes.object
        },
        render: function() {
            return React_createElement(RecordEditor,{
                entity: this.props.entity,
                entityTypeHumanName: "CCA",
                entityTypeMachineName: "ccas"
            },React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "name"
            },"CCA Name"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "name",
                placeholder: "e.g. Infocomm Club",
                defaultValue: (this.props.entity ?
                    (this.props.entity).name :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "category"
            },"CCA Category"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "category",
                placeholder: "e.g. Clubs and Societies",
                defaultValue: (this.props.entity ?
                    (this.props.entity).category :
                    "")
            })));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        CcaEditor: "displayName"
    })));

    var SubjectEditor = React_createClass(_.defaults({
        propTypes: {
            entity: React_PropTypes.object
        },
        getInitialState: function() {
            return {
                compulsory: ((!this.props.entity) ?
                    false :
                    (this.props.entity.code === null))
            };
        },
        render: function() {
            var that = this;
            var onCompulsoryChanged = function(event) {
                return that.setState({
                    compulsory: event.target.checked
                });
            };
            return React_createElement(RecordEditor,{
                entity: this.props.entity,
                entityTypeHumanName: "Subject",
                entityTypeMachineName: "subjects"
            },React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "name"
            },"Subject Name"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "name",
                placeholder: "e.g. Mathematics (H3)",
                defaultValue: (this.props.entity ?
                    (this.props.entity).name :
                    "")
            }),React_createElement("div",{
                className: "checkbox"
            },React_createElement("label",{},React_createElement("input",{
                type: "checkbox",
                onChange: onCompulsoryChanged,
                checked: this.state.compulsory
            }),"This is a compulsory subject.")),React_createElement("div",{
                className: "checkbox"
            },React_createElement("label",{},React_createElement("input",{
                type: "checkbox",
                name: "science",
                defaultChecked: (this.props.entity ?
                    this.props.entity.is_science :
                    false)
            }),"This is a science subject."))),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "code"
            },"Subject Code"),(this.state.compulsory ?
                React_createElement("input",{
                    type: "text",
                    className: "form-control",
                    disabled: true,
                    value: "",
                    placeholder: "None"
                }) :
                React_createElement("input",{
                    type: "text",
                    className: "form-control",
                    name: "code",
                    placeholder: "e.g. MA(H3)",
                    defaultValue: (this.props.entity ?
                        (this.props.entity).code :
                        "")
                })),React_createElement("p",{
                className: "help-block"
            },"Compulsory subjects do not have a subject code, because since everyone takes them, there is no reason to specify them in uploaded CSV/TSV/TXT files. They will, however, still appear on PDF files if they are also science subjects.")),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "level"
            },"Applies To"),React_createElement("div",{
                className: "checkbox"
            },__map([
                1,
                2,
                3,
                4,
                5,
                6
            ],function(lv) {
                var checked = (that.props.entity ?
                    (-1 !== _.indexOf(that.props.entity.level,lv)) :
                    false);
                return React_createElement("label",{
                    key: lv,
                    className: "checkbox-inline"
                },React_createElement("input",{
                    type: "checkbox",
                    name: "level",
                    value: lv,
                    defaultChecked: checked
                }),"Year ",lv);
            }))),React_createElement("div",{
                className: "checkbox"
            },React_createElement("label",{},React_createElement("input",{
                type: "checkbox",
                name: "force",
                defaultChecked: false
            }),"Force the operation to continue despite errors (not recommended).")));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        SubjectEditor: "displayName"
    })));

    class BatchUpload extends React_Component {
        constructor(props) {
            super(props);
            this.displayName = 'BatchUpload';
        }
        render() {
            let dataSpec = pageSpec[window.location.pathname].dataSpec;
            let ajaxParam = () => ({
                url: '/api/' + dataSpec.machineName + '/csv',
                type: 'POST',
                data: new window.FormData(this.refs.uploaderForm),
                contentType: false,
                processData: false
            });
            return React_createElement(AjaxFailableActionModal,{
                actionButtonStyle: "primary",
                actionButtonLabel: "Upload",
                title: "Add " + dataSpec.humanNamePlural + " via Uploading CSV/TSV/TXT File",
                ajaxParam: ajaxParam
            },React_createElement("form",{
                id: "uploaderForm",
                ref: "uploaderForm",
                role: "form"
            },React_createElement("p",{
                className: "help-block"
            },"This allows you to upload a CSV/TSV/TXT file of " + dataSpec.humanNamePluralInSentence + " and add all of them. This is an all-or-nothing operation: even if only one " + dataSpec.humanNameInSentence + " could not be added, none of the " + dataSpec.humanNamePluralInSentence + " will be added."),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "csv"
            },"CSV/TSV/TXT File"),React_createElement("input",{
                type: "file",
                className: "form-control",
                name: "csv",
                accept: "text/csv,.csv,text/plain,.txt,text/tab-separated-values,.tsv",
                required: true
            }))));
        }
    }

    var TestDecoder = React_createClass(_.defaults({
        getInitialState: function() {
            return {
                decodeResult: null
            };
        },
        render: function() {
            var that = this;
            var next = function(hideModal,setSpinner) {
                setSpinner(1);
                return $.getJSON("/api/subjects/test-decode",($("#decoderForm")).serialize(),function(data) {
                    setSpinner(0);
                    return that.setState({
                        decodeResult: data.data
                    });
                });
            };
            var subjectsToString = function(ss) {
                return (__map(ss,function(s) {
                    return s.name;
                })).join(", ");
            };
            return React_createElement(ActionModal,{
                actionButtonStyle: "primary",
                actionButtonLabel: "Decode",
                title: "Test Decode Subject Codes",
                next: next
            },(this.state.decodeResult ?
                React_createElement("div",{
                    className: "panel panel-default"
                },React_createElement("div",{
                    className: "panel-heading"
                },"Decode Results"),React_createElement("div",{
                    className: "panel-body"
                },((0 === this.state.decodeResult.length) ?
                    "The subject codes could not be decoded at all." :
                    ((1 === this.state.decodeResult.length) ?
                        React_createElement("div",{},"The subject codes could be unambiguously decoded: ",React_createElement("br",{}),subjectsToString(this.state.decodeResult[0])) :
                        (true ?
                            React_createElement("div",{},"The subject codes could not be unambiguously decoded; here are the possibilities:",React_createElement("ul",{},__map(this.state.decodeResult,function(ss) {
                                return React_createElement("li",{},subjectsToString(ss));
                            }))) :
                            undefined))))) :
                null),React_createElement("form",{
                id: "decoderForm",
                role: "form"
            },React_createElement("p",{
                className: "help-block"
            },"This decoder allows you to preview decoding of a set of subject codes. You can enter a series of subject codes and see how it will be decoded."),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "level"
            },"Year"),React_createElement("div",{
                className: "radio"
            },__map([
                1,
                2,
                3,
                4,
                5,
                6
            ],function(lv) {
                return React_createElement("label",{
                    key: lv,
                    className: "checkbox-inline"
                },React_createElement("input",{
                    type: "radio",
                    name: "level",
                    value: lv,
                    defaultChecked: (1 === lv)
                }),"Year ",lv);
            }))),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "str"
            },"Subject Code Combination"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "str",
                placeholder: "Enter subject codes here"
            }))));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        TestDecoder: "displayName"
    })));

    var TeacherEditor = React_createClass(_.defaults({
        propTypes: {
            entity: React_PropTypes.object
        },
        render: function() {
            return React_createElement(RecordEditor,{
                entity: this.props.entity,
                entityTypeHumanName: "Teacher",
                entityTypeMachineName: "teachers"
            },React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "name"
            },"Teacher Name"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "name",
                placeholder: "e.g. Chow Ban Hoe",
                defaultValue: (this.props.entity ?
                    (this.props.entity).name :
                    "")
            }),React_createElement("div",{
                className: "checkbox"
            },React_createElement("label",{},React_createElement("input",{
                type: "checkbox",
                name: "admin",
                defaultChecked: (this.props.entity ?
                    this.props.entity.is_admin :
                    false)
            }),"This teacher is an administrator."))),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "witness"
            },"Witness Name (Capital, with Salutation)"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "witness",
                placeholder: "e.g. MR CHOW BAN HOE",
                defaultValue: (this.props.entity ?
                    (this.props.entity).witness_name :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "email"
            },"Email Address"),React_createElement("input",{
                type: "email",
                className: "form-control",
                name: "email",
                placeholder: "e.g. chow_ban_hoe@moe.edu.sg",
                defaultValue: (this.props.entity ?
                    (this.props.entity).email :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "unit"
            },"Unit"),React_createElement("input",{
                type: "email",
                className: "form-control",
                name: "unit",
                placeholder: "e.g. Bio",
                defaultValue: (this.props.entity ?
                    (this.props.entity).unit :
                    "")
            })));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        TeacherEditor: "displayName"
    })));

    var StudentEditor = React_createClass(_.defaults({
        propTypes: {
            auxiliary: React_PropTypes.object.isRequired,
            entity: React_PropTypes.object
        },
        getInitialState: function() {
            return {
                currentLevel: (this.props.entity ?
                    this.props.entity["class"][0] :
                    undefined)
            };
        },
        render: function() {
            var that = this;
            var classChange = function(e) {
                var match = (/^([1-6])[A-NP-Z]$/).exec(e.target.value);
                return that.setState({
                    currentLevel: (match ?
                        parseInt(match[1],10) :
                        null)
                });
            };
            return React_createElement(RecordEditor,{
                entity: this.props.entity,
                entityTypeHumanName: "Student",
                entityTypeMachineName: "students"
            },React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "class"
            },"Class"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "class",
                placeholder: "e.g. 5N",
                defaultValue: (this.props.entity ?
                    (this.props.entity["class"]).join("") :
                    ""),
                onChange: classChange
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "indexno"
            },"Register Number"),React_createElement("input",{
                type: "number",
                className: "form-control",
                name: "indexno",
                placeholder: "e.g. 22",
                defaultValue: (this.props.entity ?
                    (this.props.entity).index_number :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "name"
            },"Full Name"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "name",
                inputMode: "latin-name",
                defaultValue: (this.props.entity ?
                    (this.props.entity).name :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "chinesename"
            },"Chinese Name"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "chinesename",
                inputMode: "kana",
                defaultValue: (this.props.entity ?
                    (this.props.entity).chinese_name :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "nric"
            },"Partial NRIC"),React_createElement("input",{
                type: "text",
                className: "form-control",
                name: "nric",
                inputMode: "verbatim",
                defaultValue: (this.props.entity ?
                    (this.props.entity).nric :
                    "")
            })),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "witnesser"
            },"Witness"),React_createElement("select",{
                className: "form-control",
                name: "witness",
                defaultValue: (this.props.entity ?
                    (this.props.entity).witnesser :
                    "")
            },([
                React_createElement("option",{
                    key: 0,
                    value: ""
                },"None")
            ]).concat(__map(this.props.auxiliary.teacherInfo.data,function(teacher) {
                return React_createElement("option",{
                    value: teacher.id,
                    key: teacher.id
                },teacher.name," (",teacher.witness_name,")");
            })))),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "subj"
            },"Subject Combination"),React_createElement("div",{
                className: "checkbox"
            },(this.state.currentLevel ?
                __map(_.filter(that.props.auxiliary.subjectInfo.data,function(subject) {
                    return (_.contains(subject.level,that.state.currentLevel) && (!(subject.code === null)));
                }),function(subject,idx) {
                    return React_createElement("label",{
                        className: "checkbox-inline",
                        key: subject.id
                    },React_createElement("input",{
                        type: "checkbox",
                        name: "subj",
                        value: (1 + idx),
                        defaultChecked: (that.props.entity ?
                            _.contains(that.props.entity.subject_combi,subject.id) :
                            false)
                    }),subject.name," (",subject.code,")");
                }) :
                React_createElement("p",{
                    className: "help-block"
                },"Subjects are not available for selection because you did not correctly enter a class.")))));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        StudentEditor: "displayName"
    })));

    var DeleteConfirmation = React_createClass(_.defaults({
        propTypes: {
            entity: React_PropTypes.object.isRequired
        },
        render: function() {
            var dataSpec = (pageSpec[window.location.pathname]).dataSpec;
            var hname = dataSpec.humanName;
            var mname = dataSpec.machineName;
            var endpoint = ((("/api/" + mname) + "/") + this.props.entity.id);
            var ajaxParam = function() {
                return {
                    url: endpoint,
                    type: "DELETE"
                };
            };
            return React_createElement(AjaxFailableActionModal,{
                title: ("Delete " + hname),
                actionButtonLabel: "Delete",
                actionButtonStyle: "danger",
                ajaxParam: ajaxParam
            },React_createElement("p",{},"Are you sure you want to delete the ",hname," “",this.props.entity.name,"” from the database?"));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        DeleteConfirmation: "displayName"
    })));

    var AdminHomeR = React_createClass(_.defaults({
        render: function() {
            return React_createElement("div",{
                className: "row"
            },React_createElement("div",{
                className: "col-sm-11 col-md-8 col-lg-7"
            },React_createElement("h2",{},"Welcome"),React_createElement("p",{},"Welcome to the admin console for RVHS Science Lab Undertaking Project. Click Manage Students from the tab above to view information about students.",(identIsAdmin ?
                " As an administrator, you can also manage other things from the above tabs." :
                null))));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AdminHomeR: "displayName"
    })));

    var EntityPage = React_createClass(_.defaults({
        propTypes: {
            customButtons: React_PropTypes.node,
            wsUrl: React_PropTypes.string,
            auxiliary: React_PropTypes.object,
            customFilter: React_PropTypes.func
        },
        getInitialState: function() {
            var dataSpec = (pageSpec[window.location.pathname]).dataSpec;
            var mname = dataSpec.machineName;
            return {
                conn: APIConnection((this.props.wsUrl || ("/api/" + mname)))
            };
        },
        componentWillReceiveProps: function(newProps) {
            var that = this;
            return ((newProps.wsUrl !== this.props.wsUrl) ?
                (function() {
                    that.state.conn.close();
                    return that.setState({
                        conn: APIConnection(newProps.wsUrl)
                    });
                })() :
                undefined);
        },
        render: function() {
            var that = this;
            var dataSpec = (pageSpec[window.location.pathname]).dataSpec;
            var hnamepl = dataSpec.humanNamePlural;
            var mname = dataSpec.machineName;
            var editor = dataSpec.editor;
            let onBatchUploadButtonClick = function() {
                return ReactDOM.render(React_createElement(BatchUpload, {}),getModalWrapper());
            };
            var onAddButtonClick = function() {
                return ReactDOM.render(React_createElement(editor,{
                    auxiliary: that.props.auxiliary
                }),getModalWrapper());
            };
            var onRemoveAllButtonClick = function() {
                return ReactDOM.render(React_createElement(AjaxFailableActionModal,{
                    title: ("Deleting All " + hnamepl),
                    actionButtonLabel: "Yes, Delete All",
                    actionButtonStyle: "danger",
                    ajaxParam: function() {
                        return {
                            url: ("/api/" + mname),
                            type: "DELETE"
                        };
                    }
                },React_createElement("p",{},"Are you sure you want to delete all ",hnamepl," currently stored in the database? ")),getModalWrapper());
            };
            return React_createElement("div",{},(identIsAdmin ?
                React_createElement("div",{
                    className: "pull-right btn-group",
                    role: "toolbar",
                    "aria-label": "Action Buttons"
                },(this.props.customButtons ?
                    this.props.customButtons :
                   null),React_createElement("button",{
                       type: "button",
                       className: "btn btn-default",
                       onClick: onBatchUploadButtonClick
                   },"Add New (CSV/TSV/TXT File)"),React_createElement("button",{
                    type: "button",
                    className: "btn btn-default",
                    onClick: onAddButtonClick
                },"Add New"),React_createElement("button",{
                    type: "button",
                    className: "btn btn-danger",
                    onClick: onRemoveAllButtonClick
                },"Remove ", E("strong", {style: {textTransform: 'uppercase'}}, "Everything"))) :
                null),React_createElement("h2",{},("View " + hnamepl)),this.props.children,React_createElement(EntityTable,{
                conn: this.state.conn,
                entityEditor: editor,
                auxiliary: this.props.auxiliary,
                customFilter: this.props.customFilter
            }));
        },
        shouldComponentUpdate: function(newProps) {
            return (!_.isEqual(this.props,newProps));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        EntityPage: "displayName"
    })));

    var AdminCcasR = React_createClass(_.defaults({
        render: function() {
            return React_createElement(EntityPage,{});
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AdminCcasR: "displayName"
    })));

    var AdminSubjectsR = React_createClass(_.defaults({
        render: function() {
            var onTestDecodeButtonClick = function() {
                return ReactDOM.render(React_createElement(TestDecoder,{}),getModalWrapper());
            };
            var customButtons = React_createElement("button",{
                onClick: onTestDecodeButtonClick,
                type: "button",
                className: "btn btn-default"
            },"Test Decode");
            return React_createElement(EntityPage,{
                customButtons: customButtons
            });
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AdminSubjectsR: "displayName"
    })));

    var AdminTeachersR = React_createClass(_.defaults({
        render: function() {
            return React_createElement(EntityPage,{});
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AdminTeachersR: "displayName"
    })));

    class AdminStudentsR extends React_Component {
        constructor(props) {
            let ccaConn = APIConnection("/api/ccas");
            let teacherConn = APIConnection("/api/teachers");
            let subjectConn = APIConnection("/api/subjects");
            let classConn = APIConnection("/api/classes");

            ccaConn.registerCallback((d) => {
                this.setState({ccaInfo: d});
                this.reserializeForm();
            });
            teacherConn.registerCallback((d) => {
                this.setState({teacherInfo: d});
                this.reserializeForm();
            });
            subjectConn.registerCallback((d) => {
                this.setState({subjectInfo: d});
                this.reserializeForm();
            });
            classConn.registerCallback((d) => {
                this.setState({classInfo: d});
                this.reserializeForm();
            });
            let emptyData = {data: []};
            this.state = {
                queryString: "searchby=none",
                selected: "class",
                hideWithoutWitness: true,
                ccaConn: ccaConn,
                teacherConn: teacherConn,
                subjectConn: subjectConn,
                ccaInfo: emptyData,
                teacherInfo: emptyData,
                subjectInfo: emptyData,
                classInfo: emptyData
            };
        }

        reserializeForm() {
            let queryString = $(this.refs.searchbyForm).serialize();
            // Those are derivative data, so if all students are deleted, we
            // need to reset the queryString to its initial state.
            if (queryString === "searchby=class" || queryString === "searchby=level")
                this.setState({queryString: "searchby=none"});
            else
                this.setState({queryString});
        }

        componentDidUpdate(prevProps, prevState) {
            if (prevState.selected !== this.state.selected)
                this.reserializeForm();
        }

        componentWillUnmount() {
            this.state.ccaConn.close();
            this.state.teacherConn.close();
            this.state.subjectConn.close();
            this.state.classConn.close();
        }

        render() {
            let onRadioChange = (e) => {
                if (e.target.checked)
                    this.setState({selected: e.target.value});
            };
            let onCheckClick = (e) => {this.setState({hideWithoutWitness: e.target.checked});};
            let customFilter = (data) => this.state.hideWithoutWitness ? _.filter(data, (d) => d.witnesser) : data;
            let auxiliary = {
                teacherInfo: this.state.teacherInfo,
                ccaInfo: this.state.ccaInfo,
                subjectInfo: this.state.subjectInfo,
                classInfo: this.state.classInfo
            };
            let mainControlSize = "col-sm-9 col-md-7 col-lg-6 ";
            let labelSize = "col-sm-3 col-md-2 col-lg-2 ";
            let labelOffset = "col-md-offset-1 col-lg-offset-1 ";
            let offset = "col-sm-offset-3 col-md-offset-3 col-lg-offset-3 ";

            return E("div", {},
                     E("div", {className: "row"},
                       E("h4", {className: offset + ' ' + mainControlSize},
                         "Which students would you like to see?")),
                     E("form", {role: "form", ref: "searchbyForm", className: "form-horizontal"},
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "class", defaultChecked: true, onChange: onRadioChange}),
                           "Search By Class"),
                         E("div", {className: mainControlSize},
                           this.state.selected === "class" ?
                           E("select", {className: "form-control", name: "class", onChange: this.reserializeForm.bind(this)},
                             __map(auxiliary.classInfo.data,
                                   (klass) => {
                                       let klassstr = (klass[0] + klass[1]);
                                       return E("option", {value: klassstr, key: klassstr}, klassstr);
                                   })) :
                           E("select", {className: "form-control", disabled: true}))),
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "level", defaultChecked: false, onChange: onRadioChange}),
                           "Search By Level"),
                         E("div", {className: mainControlSize},
                           this.state.selected === "level" ?
                           E("select", {className: "form-control", name: "level", onChange: this.reserializeForm.bind(this)},
                             __map(_.uniq(auxiliary.classInfo.data, false, (k) => k[0]),
                                   (klass) => E("option", {value: klass[0], key: klass[0]}, "Year ", klass[0]))) :
                           E("select", {className: "form-control", disabled: true}))),
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "name", defaultChecked: false, onChange: onRadioChange}),
                           "Search By Approx. Name"),
                         E("div", {className: mainControlSize},
                           (this.state.selected === "name" ?
                            E("input", {type: "text", className: "form-control", name: "name", placeholder: "Enter an approximate name, e.g. Xin Yi", onChange: this.reserializeForm.bind(this)}) :
                            E("input", {type: "text", className: "form-control", disabled: true})))),
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "subject", defaultChecked: false, onChange: onRadioChange}),
                           "Search By Subject"),
                         E("div", {className: mainControlSize},
                           (this.state.selected === "subject" ?
                            E("select", {className: "form-control", name: "id", onChange: this.reserializeForm.bind(this)},
                              __map(_.sortBy(auxiliary.subjectInfo.data, (subject) => subject.name), (subject) =>
                                    E("option", {value: subject.id, key: subject.id},
                                      subject.name,
                                      " (",
                                      __map(subject.level, (l) => "Year " + l).join(", "),
                                      ")"))) :
                            E("select", {className: "form-control", disabled: true})))),
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "cca", defaultChecked: false, onChange: onRadioChange}),
                           "Search By CCA"),
                         E("div", {className: mainControlSize},
                           (this.state.selected === "cca" ?
                            E("select", {className: "form-control", name: "id", onChange: this.reserializeForm.bind(this)},
                              __map(auxiliary.ccaInfo.data, (cca) =>
                                    E("option", {value: cca.id, key: cca.id},
                                      cca.name,
                                      " (", cca.category, ")"))) :
                            E("select", {className: "form-control", disabled: true})))),
                       E("div", {className: "radio form-group"},
                         E("label", {className: labelOffset + labelSize + "control-label"},
                           E("input", {type: "radio", name: "searchby", value: "teacher", defaultChecked: false, onChange: onRadioChange}),
                           "Search By Witness"),
                         E("div", {className: mainControlSize},
                           (this.state.selected === "teacher" ?
                            E("select", {className: "form-control", name: "id", onChange: this.reserializeForm.bind(this)},
                              __map(auxiliary.teacherInfo.data, (teacher) =>
                                    E("option", {
                                        value: teacher.id,
                                        key: teacher.id
                                    }, teacher.name, " (", teacher.witness_name, ")"))) :
                            E("select", {className: "form-control", disabled: true})))),
                       E("div", {className: "form-group"},
                         E("div", {className: offset + ' ' + mainControlSize},
                           E("div", {className: "checkbox"},
                             E("label", {}, E("input", {type: "checkbox", defaultChecked: true, onChange: onCheckClick}),
                               " Hide Students Without Witness"))))),
                     E("div", {className: "row"},
                       E(EntityPage, {wsUrl: "/api/students?" + this.state.queryString, auxiliary: auxiliary, customFilter: customFilter})));
        }
    }

    var lookupForeign = function(dataset,id) {
        return (_.find(dataset.data,function(v) {
            return (id === v.id);
        }) || "??");
    };

    var SubmissionCompleteModal = React_createClass(_.defaults({
        propTypes: {
            sub: React_PropTypes.object.isRequired,
            ccaInfo: React_PropTypes.object.isRequired,
            onLockClick: React_PropTypes.func.isRequired,
            onUnlockClick: React_PropTypes.func.isRequired
        },
        render: function() {
            var that = this;
            var button = React_createElement("button",{
                type: "button",
                className: "btn btn-default",
                "data-dismiss": "modal"
            },"OK");
            var onLockClick = function() {
                that.props.onLockClick();
                return dismissModal();
            };
            var onUnlockClick = function() {
                that.props.onUnlockClick();
                return dismissModal();
            };
            return React_createElement(Modal,{
                canClose: true,
                title: "Student-Submitted Information",
                buttons: button
            },React_createElement("table",{
                className: "table"
            },React_createElement("tbody",{},React_createElement("tr",{},React_createElement("th",{},"Email"),React_createElement("td",{},this.props.sub.email)),React_createElement("tr",{},React_createElement("th",{},"Phone Number"),React_createElement("td",{},this.props.sub.phone)),React_createElement("tr",{},React_createElement("th",{},"Date of Submission"),React_createElement("td",{},this.props.sub.date)),React_createElement("tr",{},React_createElement("th",{},"CCAs"),React_createElement("td",{},((__map(this.props.sub.cca,function(s) {
                return (lookupForeign(that.props.ccaInfo,s)).name;
            })).join(", ") || "None"))),React_createElement("tr",{},React_createElement("th",{},"Delete Submission"),React_createElement("td",{},React_createElement("div",{
                className: "btn-toolbar",
                role: "toolbar"
            },React_createElement("button",{
                type: "button",
                className: "btn btn-default",
                onClick: onUnlockClick
            },"Delete and Unlock"),React_createElement("button",{
                type: "button",
                className: "btn btn-default",
                onClick: onLockClick
            },"Delete and Lock")))))));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        SubmissionCompleteModal: "displayName"
    })));

    var pageSpec = {
        "/admin": {
            pageName: "Home",
            onlyAdmin: false,
            component: AdminHomeR,
            dataSpec: null
        },

        "/admin/ccas": {
            pageName: "Manage CCAs",
            onlyAdmin: false,
            component: AdminCcasR,
            dataSpec: {
                humanName: "CCA",
                humanNamePlural: "CCAs",
                humanNameInSentence: "CCA",
                humanNamePluralInSentence: "CCAs",
                machineName: "ccas",
                editor: CcaEditor,
                categoryColumn: [
                    "category",
                    _.identity,
                    "CCA Category"
                ],
                columns: [
                    [
                        "name",
                        _.identity,
                        "CCA Name"
                    ]
                ]
            }
        },

        "/admin/subjects": {
            pageName: "Manage Subjects",
            onlyAdmin: false,
            component: AdminSubjectsR,
            dataSpec: {
                humanName: "Subject",
                humanNamePlural: "Subjects",
                humanNameInSentence: "subject",
                humanNamePluralInSentence: "subjects",
                machineName: "subjects",
                editor: SubjectEditor,
                categoryColumn: [
                    "level",
                    function(ls) {
                        return (__map(ls,function(l) {
                            return ("Year " + l);
                        })).join(", ");
                    },
                    "Applies To"
                ],
                columns: [
                    [
                        "name",
                        _.identity,
                        "Subject Name"
                    ],
                    [
                        "code",
                        function(v) {
                            return (v ?
                                React_createElement("code",{},v) :
                                React_createElement("i",{},"(None; Compulsory Subject)"));
                        },
                        "Subject Code"
                    ],
                    [
                        "is_science",
                        function(b) {
                            return (b ?
                                "Yes" :
                                "No");
                        },
                        "Science Subject?"
                    ]
                ]
            }
        },

        "/admin/teachers": {
            pageName: "Manage Teachers",
            onlyAdmin: true,
            component: AdminTeachersR,
            dataSpec: {
                humanName: "Teacher",
                humanNamePlural: "Teachers",
                humanNameInSentence: "teacher",
                humanNamePluralInSentence: "teachers",
                machineName: "teachers",
                editor: TeacherEditor,
                categoryColumn: null,
                columns: [
                    [
                        "name",
                        _.identity,
                        "Name"
                    ],
                    [
                        "unit",
                        _.identity,
                        "Unit"
                    ],
                    [
                        "email",
                        _.identity,
                        "Email Address"
                    ],
                    [
                        "is_admin",
                        function(b) {
                            return (b ?
                                "Yes" :
                                "No");
                        },
                        "Administrator?"
                    ],
                    [
                        "witness_name",
                        _.identity,
                        "Witness Name"
                    ]
                ]
            }
        },

        "/admin/students": {
            pageName: "Manage Students",
            onlyAdmin: false,
            component: AdminStudentsR,
            dataSpec: {
                humanName: "Student",
                humanNamePlural: "Students",
                humanNameInSentence: "student",
                humanNamePluralInSentence: "students",
                machineName: "students",
                editor: StudentEditor,
                categoryColumn: [
                    "class",
                    _.identity,
                    "Class"
                ],
                columns: [
                    [
                        "index_number",
                        _.identity,
                        "Index #"
                    ],
                    [
                        "name",
                        _.identity,
                        "Name"
                    ],
                    [
                        "chinese_name",
                        _.identity,
                        "Chinese"
                    ],
                    [
                        "nric",
                        function(v) {
                            return (v).slice(-5);
                        },
                        "ID"
                    ],
                    [
                        "subject_combi",
                        function(ss) {
                            var that = this;
                            var codes = (__map(ss,function(s) {
                                return (lookupForeign(that.subjectInfo,s)).code;
                            })).join(", ");
                            var names = (__map(ss,function(s) {
                                return (lookupForeign(that.subjectInfo,s)).name;
                            })).join(", ");
                            return (codes ?
                                React_createElement("span",{
                                    title: names
                                },codes) :
                                "—");
                        },
                        "Subject(s)"
                    ],
                    [
                        "witnesser",
                        function(tid) {
                            return ((tid === null) ?
                                React_createElement("i",{},"None") :
                                (lookupForeign(this.teacherInfo,tid)).name);
                        },
                        "Witness"
                    ],
                    [
                        "submission",
                        function(sub) {
                            return (sub.email || "—");
                        },
                        "Email"
                    ],
                    [
                        "submission",
                        function(sub) {
                            return (sub.phone || "—");
                        },
                        "Phone"
                    ],
                    [
                        "submission",
                        function(sub) {
                            return (sub.final_declaration_filename ?
                                React_createElement("a",{
                                    className: "btn btn-default btn-xs",
                                    target: "_blank",
                                    href: ("https://rvhs-sci-lab-undertaking.appspot.com/storage?filename=" + encodeURIComponent(sub.final_declaration_filename))
                                },React_createElement("span",{
                                    className: "glyphicon glyphicon-floppy-save",
                                    "aria-hidden": "true"
                                })) :
                                ((sub.tag === "SubmissionCompleted") ?
                                    React_createElement("img",{
                                        src: "/static/res/loading.gif",
                                        width: 16,
                                        height: 16,
                                        style: {
                                            margin: "0 4px"
                                        }
                                    }) :
                                    undefined));
                        },
                        "PDF"
                    ],
                    [
                        "submission",
                        function(sub,entity) {
                            var that = this;
                            var lockicon = React_createElement("span",{
                                className: "glyphicon glyphicon-lock",
                                "aria-hidden": "true"
                            });
                            var completeicon = React_createElement("span",{
                                className: "glyphicon glyphicon-ok",
                                "aria-hidden": "true"
                            });
                            var onUnlockClick = function() {
                                return $.ajax((("/api/students/" + entity.id) + "/unlock"),{
                                    type: "POST"
                                });
                            };
                            var onLockClick = function() {
                                return $.ajax((("/api/students/" + entity.id) + "/lock"),{
                                    type: "POST"
                                });
                            };
                            var onCompleteClick = function() {
                                return ReactDOM.render(React_createElement(SubmissionCompleteModal,{
                                    ccaInfo: that.ccaInfo,
                                    sub: sub,
                                    onLockClick: onLockClick,
                                    onUnlockClick: onUnlockClick
                                }),getModalWrapper());
                            };
                            return React_createElement("div",{
                                className: "btn-group",
                                role: "toolbar",
                                "aria-label": "Status Buttons"
                            },((sub.tag === "SubmissionNotOpen") ?
                                React_createElement("button",{
                                    type: "button",
                                    className: "btn btn-warning btn-xs",
                                    onClick: onUnlockClick,
                                    title: "Click to unlock submission."
                                },lockicon,React_createElement("span",{
                                    className: "presentation-text",
                                    "data-text": " Locked"
                                })) :
                                ((sub.tag === "SubmissionOpen") ?
                                    React_createElement("button",{
                                        type: "button",
                                        className: "btn btn-primary btn-xs",
                                        onClick: onLockClick,
                                        title: "Click to lock submission."
                                    },lockicon,React_createElement("span",{
                                        className: "presentation-text",
                                        "data-text": " Unlocked"
                                    })) :
                                    ((sub.tag === "SubmissionCompleted") ?
                                        React_createElement("button",{
                                            type: "button",
                                            className: "btn btn-success btn-xs",
                                            onClick: onCompleteClick,
                                            title: "Click to view submitted information."
                                        },completeicon,React_createElement("span",{
                                            className: "presentation-text",
                                            "data-text": " Completed"
                                        })) :
                                        undefined))));
                        },
                        function(rawData) {
                            var lockicon = React_createElement("span",{
                                className: "glyphicon glyphicon-lock",
                                "aria-hidden": "true"
                            });
                            var completeicon = React_createElement("span",{
                                className: "glyphicon glyphicon-ok",
                                "aria-hidden": "true"
                            });
                            var onUnlockAllClick = function() {
                                var ids = (__map(rawData,"id")).join(",");
                                return $.ajax("/api/students/many/unlock",{
                                    type: "POST",
                                    data: {
                                        ids: ids
                                    }
                                });
                            };
                            var onLockAllClick = function() {
                                var ids = (__map(rawData,"id")).join(",");
                                return $.ajax("/api/students/many/lock",{
                                    type: "POST",
                                    data: {
                                        ids: ids
                                    }
                                });
                            };
                            return React_createElement("span",{},React_createElement("button",{
                                type: "button",
                                className: "btn btn-primary btn-xs",
                                onClick: onUnlockAllClick,
                                title: "Click to unlock submission for all."
                            },lockicon,React_createElement("span",{
                                className: "presentation-text",
                                "data-text": " Unlock These"
                            }))," ",React_createElement("button",{
                                type: "button",
                                className: "btn btn-warning btn-xs",
                                onClick: onLockAllClick,
                                title: "Click to lock submission for all."
                            },lockicon,React_createElement("span",{
                                className: "presentation-text",
                                "data-text": " Lock These"
                            })));
                        }
                    ]
                ]
            }
        }
    };

    var Page = React_createClass(_.defaults({
        render: function() {
            var pathname = window.location.pathname;
            var tabs = __map(pageSpec,function(page,route) {
                var tab = React_createElement("li",{
                    key: route,
                    role: "presentation",
                    className: ((route === pathname) ?
                        "active" :
                        "")
                },React_createElement("a",{
                    href: ((route === pathname) ?
                        "#" :
                        route)
                },(page).pageName));
                return ((page).onlyAdmin ?
                    (identIsAdmin ?
                        tab :
                        null) :
                    tab);
            });
            return React_createElement("div",{
                id: "content-wrapper"
            },React_createElement("div",{
                id: "popover-wrapper"
            }),React_createElement("div",{
                id: "modal-wrapper"
            }),React_createElement("div",{
                className: "container"
            },React_createElement("div",{
                className: "page-header"
            },React_createElement("h1",{},React_createElement("img",{
                src: "/static/res/rv.svg",
                style: {
                    height: "1em",
                    position: "relative",
                    top: "-0.2em",
                    margin: "0 0.3em 0 0"
                }
            }),"RVHS Science Lab Undertaking — For Teachers")),React_createElement("p",{},(("You are logged in as " + identUser) + ". "),(identIsAdmin ?
                "You are an administrator. " :
                "You are not an administrator, and many features and pages have been hidden."),React_createElement("a",{
                href: "/auth/logout"
            },"Click here to logout. ")),React_createElement("div",{
                role: "tabpanel"
            },React_createElement("ul",{
                className: "nav nav-tabs"
            },tabs)),React_createElement("div",{
                id: "main-content"
            })));
        },
        componentDidMount: function() {
            var pathname = window.location.pathname;
            return ((typeof(window.EventSource) === "undefined") ?
                ReactDOM.render(React_createElement(Modal,{
                    canClose: true,
                    title: "Browser Unsupported"
                },React_createElement("p",{},"Your browser is unsupported. Although you may still be able to use this site, it is ", React_createElement("b",{},"highly recommended")," that you use a different browser, such as Google Chrome 9 or higher, Mozilla Firefox 6 or higher, or Apple Safari 5 or higher. Internet Explorer is known to behave inconsistently and therefore should not be used.")),getModalWrapper()) :
                ReactDOM.render(React_createElement((pageSpec[pathname]).component,{}),($("#main-content")).get(0)));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        Page: "displayName"
    })));

    return ReactDOM.render(React_createElement(Page,{}),document.getElementById('body'));
});
})(window, document, React, ReactDOM, $, _);


// Local Variables:
// eval: (add-hook (quote after-save-hook) (lambda nil (shell-command "es6c admin.js > ../static/admin.min.js")) nil t)
// End:
