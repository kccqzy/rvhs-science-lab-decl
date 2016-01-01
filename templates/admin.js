// -*- js2-basic-offset: 4; -*-
"use strict";
$(function() {
    var React_createElement = React.createElement;
    var React_createClass = React.createClass;
    var React_PropTypes = React.PropTypes;
    var __map = _.map;
    var identUser = ($("#meta-user")).attr("value");
    var identPriv = ($("#meta-priv")).attr("value");
    var APIConnection = function(pathname) {
        var wsUrl = ((((window.location.protocol === "https:") ?
            "wss://" :
            "ws://") + window.location.host) + pathname);
        var conn = null;
        var callback = null;
        var timeConnected = null;
        var connect = function() {
            return (((Date.now() - timeConnected) > 2000) ?
                (function() {
                    timeConnected = Date.now();
                    conn = new WebSocket(wsUrl);
                    conn.onmessage = callback;
                    conn.onerror = function() {
                        console.log("WS connection errored. Retrying.");
                        return connect();
                    };
                    conn.onclose = function(e) {
                        console.log(e);
                        console.log("WS connection closed without request from client. Retrying.");
                        return connect();
                    };
                })() :
                setTimeout(connect,(Date.now() - timeConnected)));
        };
        var close = function() {
            conn.onmessage = _.noop;
            conn.onerror = _.noop;
            conn.onclose = _.noop;
            callback = null;
            return conn.close();
        };
        ($(window)).on("beforeunload",function() {
            return close();
        });
        return {
            registerCallback: function(func) {
                ((!conn) ?
                    connect() :
                    _.noop());
                var wrapFunc = function(e) {
                    return func(JSON.parse(e.data));
                };
                conn.onmessage = wrapFunc;
                callback = wrapFunc;
            },
            pathname: function() {
                return pathname;
            },
            close: close
        };
    };
    var EntityRow = React_createClass(_.defaults({
        propTypes: {
            firstRowSpan: React_PropTypes.number.isRequired,
            entity: React_PropTypes.object.isRequired,
            entityEditor: React_PropTypes.any.isRequired,
            auxiliary: React_PropTypes.object
        },
        render: function() {
            var that = this;
            var dataSpec = (pageSpec[window.location.pathname]).dataSpec;
            var firstCell = (((!(dataSpec.categoryColumn === null)) && that.props.firstRowSpan) ?
                (function() {
                    var value = that.props.entity[dataSpec.categoryColumn[0]];
                    var mapper = dataSpec.categoryColumn[1];
                    return React_createElement("td",{
                        rowSpan: that.props.firstRowSpan
                    },mapper.apply(that.props.auxiliary,[
                        value,
                        that.props.entity
                    ]));
                })() :
                null);
            var onEditButtonClick = function() {
                return React.render(React_createElement(that.props.entityEditor,{
                    auxiliary: that.props.auxiliary,
                    entity: that.props.entity
                }),getModalWrapper());
            };
            var onDeleteButtonClick = function() {
                return React.render(React_createElement(DeleteConfirmation,{
                    auxiliary: that.props.auxiliary,
                    entity: that.props.entity
                }),getModalWrapper());
            };
            return React_createElement("tr",{},firstCell,__map(dataSpec.columns,function(spec,idx) {
                var value = that.props.entity[spec[0]];
                var mapper = spec[1];
                return React_createElement("td",{
                    key: idx
                },mapper.apply(that.props.auxiliary,[
                    value,
                    that.props.entity
                ]));
            }),((identPriv === "PrivAdmin") ?
                React_createElement("td",{
                    className: "text-right"
                },React_createElement("div",{
                    className: "btn-group",
                    role: "group"
                },React_createElement("button",{
                    type: "button",
                    className: "btn btn-default btn-xs",
                    title: "Edit",
                    onClick: onEditButtonClick
                },React_createElement("span",{
                    className: "glyphicon glyphicon-pencil",
                    "aria-hidden": "true"
                })),React_createElement("button",{
                    type: "button",
                    className: "btn btn-default btn-xs",
                    title: "Delete",
                    onClick: onDeleteButtonClick
                },React_createElement("span",{
                    className: "glyphicon glyphicon-trash",
                    "aria-hidden": "true"
                })))) :
                null));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        EntityRow: "displayName"
    })));
    var EntityCategory = React_createClass(_.defaults({
        propTypes: {
            entities: React_PropTypes.array.isRequired,
            entityEditor: React_PropTypes.any.isRequired,
            auxiliary: React_PropTypes.object
        },
        render: function() {
            var that = this;
            return React_createElement("tbody",{},__map(this.props.entities,function(entity,i,entities) {
                return React_createElement(EntityRow,{
                    key: entity.id,
                    entityEditor: that.props.entityEditor,
                    auxiliary: that.props.auxiliary,
                    entity: entity,
                    firstRowSpan: (i ?
                        0 :
                        entities.length)
                });
            }));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        EntityCategory: "displayName"
    })));
    var EntityTable = React_createClass(_.defaults({
        propTypes: {
            conn: React_PropTypes.object.isRequired,
            entityEditor: React_PropTypes.any.isRequired,
            auxiliary: React_PropTypes.object,
            customFilter: React_PropTypes.func
        },
        getInitialState: function() {
            return {
                tableData: {
                    data: []
                }
            };
        },
        registerCallback: function() {
            var that = this;
            return this.props.conn.registerCallback(function(d) {
                return that.setState({
                    tableData: d
                });
            });
        },
        componentDidMount: function() {
            return this.registerCallback();
        },
        componentDidUpdate: function() {
            return this.registerCallback();
        },
        componentWillUnmount: function() {
            return this.props.conn.close();
        },
        componentWillReceiveProps: function(newProps) {
            return ((this.props.conn.pathname() !== newProps.conn.pathname()) ?
                this.props.conn.close() :
                undefined);
        },
        render: function() {
            var that = this;
            var dataSpec = (pageSpec[window.location.pathname]).dataSpec;
            var rawData = (this.props.customFilter || _.identity)(this.state.tableData.data);
            var rows = ((dataSpec.categoryColumn === null) ?
                (function() {
                    var sortName = dataSpec.columns[0][0];
                    var massagedData = _.sortBy(rawData,sortName);
                    console.log(that.state.tableData);
                    console.log(rawData);
                    console.log(massagedData);
                    return React_createElement("tbody",{},__map(massagedData,function(entity) {
                        return React_createElement(EntityRow,{
                            firstRowSpan: 1,
                            entityEditor: that.props.entityEditor,
                            auxiliary: that.props.auxiliary,
                            entity: entity,
                            key: entity.id
                        });
                    }));
                })() :
                (function() {
                    var categoryName = dataSpec.categoryColumn[0];
                    var sortName = dataSpec.columns[0][0];
                    var massagedData = __map(_.sortBy(__map(_.groupBy(rawData,categoryName),function(v,k) {
                        return {
                            k: k,
                            v: _.sortBy(v,sortName)
                        };
                    }),"k"),function(d) {
                        return (d).v;
                    });
                    return __map(massagedData,function(entities) {
                        var category = entities[0][categoryName];
                        return React_createElement(EntityCategory,{
                            entities: entities,
                            entityEditor: that.props.entityEditor,
                            auxiliary: that.props.auxiliary,
                            key: category
                        });
                    });
                })());
            var displayColumnName = function(columnName) {
                return ((Object.prototype.toString.call(columnName) === "[object Function]") ?
                    columnName(rawData) :
                    columnName);
            };
            var headers = __map((((dataSpec.categoryColumn === null) ?
                [] :
                [
                    displayColumnName(dataSpec.categoryColumn[2])
                ])).concat(__map(dataSpec.columns,function(v) {
                return displayColumnName(v[2]);
            })),function(label,idx) {
                return React_createElement("th",{
                    key: idx
                },label);
            });
            return React_createElement("div",{
                className: "table-responsive"
            },React_createElement("table",{
                className: "table"
            },React_createElement("thead",{},React_createElement("tr",{},headers,((identPriv === "PrivAdmin") ?
                React_createElement("th",{}) :
                null))),rows));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        EntityTable: "displayName"
    })));
    var Modal = React_createClass(_.defaults({
        propTypes: {
            canClose: React_PropTypes.bool.isRequired,
            title: React_PropTypes.node.isRequired,
            buttons: React_PropTypes.node,
            children: React_PropTypes.node.isRequired
        },
        render: function() {
            var header = React_createElement("div",{
                className: "modal-header"
            },(this.props.canClose ?
                React_createElement("button",{
                    type: "button",
                    className: "close",
                    id: "modalClose",
                    "data-dismiss": "modal"
                },React_createElement("span",{
                    "aria-hidden": "true"
                },"×"),React_createElement("span",{
                    className: "sr-only"
                },"Close")) :
                ""),React_createElement("h4",{
                className: "modal-title"
            },this.props.title));
            var footer = (this.props.buttons ?
                React_createElement("div",{
                    className: "modal-footer"
                },this.props.buttons) :
                "");
            return React_createElement("div",{
                id: "modal",
                className: "modal fade"
            },React_createElement("div",{
                className: "modal-dialog"
            },React_createElement("div",{
                className: "modal-content"
            },header,React_createElement("div",{
                className: "modal-body"
            },this.props.children),footer)));
        },
        componentDidMount: function() {
            return (($(this.getDOMNode())).on("hidden.bs.modal",function() {
                return React.unmountComponentAtNode(getModalWrapper());
            })).modal({
                keyboard: false,
                backdrop: "static"
            });
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        Modal: "displayName"
    })));
    var getModalWrapper = function() {
        return ($("#modal-wrapper")).get(0);
    };
    var dismissModal = function() {
        return ($("#modal")).modal("hide");
    };
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
            },"Compulsory subjects do not have a subject code, because since everyone takes them, there is no reason to specify them in CSV. They will, however, still appear on PDF files if they are also science subjects.")),React_createElement("div",{
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
    var BatchUploadStudents = React_createClass(_.defaults({
        render: function() {
            var that = this;
            var ajaxParam = function() {
                var formData = new FormData(($("#uploaderForm")).get(0));
                console.log(formData);
                return {
                    url: "/api/students/csv",
                    type: "POST",
                    data: formData,
                    contentType: false,
                    processData: false
                };
            };
            return React_createElement(AjaxFailableActionModal,{
                actionButtonStyle: "primary",
                actionButtonLabel: "Upload",
                title: "Add Students via Uploading CSV File",
                ajaxParam: ajaxParam
            },React_createElement("form",{
                id: "uploaderForm",
                role: "form"
            },React_createElement("p",{
                className: "help-block"
            },"This allows you to upload a CSV file of students and add all of them. This is an all-or-nothing operation: even if only one student could not be added, none of the students will be added."),React_createElement("div",{
                className: "form-group"
            },React_createElement("label",{
                htmlFor: "csv"
            },"CSV File"),React_createElement("input",{
                type: "file",
                className: "form-control",
                name: "csv",
                accept: "text/csv,.csv",
                required: true
            }))));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        BatchUploadStudents: "displayName"
    })));
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
                inputmode: "latin-name",
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
                inputmode: "kana",
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
                inputmode: "verbatim",
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
            },React_createElement("h2",{},"Welcome"),React_createElement("p",{},"Welcome to the admin console for RVHS Science Lab Undertaking Project. Click Manage Students from the tab above to view information about students.",((identPriv === "PrivAdmin") ?
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
            var onAddButtonClick = function() {
                return React.render(React_createElement(editor,{
                    auxiliary: that.props.auxiliary
                }),getModalWrapper());
            };
            var onRemoveAllButtonClick = function() {
                return React.render(React_createElement(AjaxFailableActionModal,{
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
            return React_createElement("div",{},((identPriv === "PrivAdmin") ?
                React_createElement("div",{
                    className: "pull-right btn-group",
                    role: "toolbar",
                    "aria-label": "Action Buttons"
                },(this.props.customButtons ?
                    this.props.customButtons :
                    null),React_createElement("button",{
                    type: "button",
                    className: "btn btn-default",
                    onClick: onAddButtonClick
                },"Add New"),React_createElement("button",{
                    type: "button",
                    className: "btn btn-default",
                    onClick: onRemoveAllButtonClick
                },"Remove All (Even If Not Shown)")) :
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
                return React.render(React_createElement(TestDecoder,{}),getModalWrapper());
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
    var AdminStudentsR = React_createClass(_.defaults({
        getInitialState: function() {
            var that = this;
            var ccaConn = APIConnection("/api/ccas");
            var teacherConn = APIConnection("/api/teachers");
            var subjectConn = APIConnection("/api/subjects");
            var classConn = APIConnection("/api/classes");
            ccaConn.registerCallback(function(d) {
                return that.setState({
                    ccaInfo: d
                });
            });
            teacherConn.registerCallback(function(d) {
                return that.setState({
                    teacherInfo: d
                });
            });
            subjectConn.registerCallback(function(d) {
                return that.setState({
                    subjectInfo: d
                });
            });
            classConn.registerCallback(function(d) {
                return that.setState({
                    classInfo: d
                });
            });
            var emptyData = {
                data: []
            };
            return {
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
        },
        componentWillUnmount: function() {
            this.state.ccaConn.close();
            this.state.teacherConn.close();
            return this.state.subjectConn.close();
        },
        render: function() {
            var onBatchUploadButtonClick = function() {
                return React.render(React_createElement(BatchUploadStudents,{}),getModalWrapper());
            };
            var customButtons = React_createElement("button",{
                onClick: onBatchUploadButtonClick,
                type: "button",
                className: "btn btn-default"
            },"Add New (Upload CSV File)");
            var that = this;
            var onRadioChange = function(e) {
                return (e.target.checked ?
                    that.setState({
                        selected: e.target.value
                    }) :
                    undefined);
            };
            var onCheckClick = function(e) {
                console.log(e.target.checked);
                return that.setState({
                    hideWithoutWitness: e.target.checked
                });
            };
            var customFilter = function(data) {
                return (that.state.hideWithoutWitness ?
                    _.filter(data,function(d) {
                        return d.witnesser;
                    }) :
                    data);
            };
            var onViewButtonClick = function(e) {
                e.preventDefault();
                return that.setState({
                    queryString: ($("#searchbyForm")).serialize()
                });
            };
            var auxiliary = {
                teacherInfo: that.state.teacherInfo,
                ccaInfo: that.state.ccaInfo,
                subjectInfo: that.state.subjectInfo,
                classInfo: that.state.classInfo
            };
            var offsetClassName = "col-sm-9 col-sm-offset-3 col-md-7 col-md-offset-2 col-lg-6 col-lg-offset-2";
            return React_createElement("div",{},React_createElement("div",{
                className: "row"
            },React_createElement("h4",{
                className: offsetClassName
            },"Which students would you like to see?")),React_createElement("form",{
                role: "form",
                id: "searchbyForm",
                className: "form-horizontal"
            },React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "class",
                defaultChecked: true,
                onChange: onRadioChange
            }),"Search By Class"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "class") ?
                React_createElement("select",{
                    className: "form-control",
                    name: "class"
                },__map(auxiliary.classInfo.data,function(klass) {
                    var klassstr = (klass[0] + klass[1]);
                    return React_createElement("option",{
                        value: klassstr,
                        key: klassstr
                    },klassstr);
                })) :
                React_createElement("select",{
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "level",
                defaultChecked: false,
                onChange: onRadioChange
            }),"Search By Level"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "level") ?
                React_createElement("select",{
                    className: "form-control",
                    name: "level"
                },__map(_.uniq(auxiliary.classInfo.data,false,function(k) {
                    return k[0];
                }),function(klass) {
                    return React_createElement("option",{
                        value: klass[0],
                        key: klass[0]
                    },"Year ",klass[0]);
                })) :
                React_createElement("select",{
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "name",
                defaultChecked: false,
                onChange: onRadioChange
            }),"Search By Approx. Name"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "name") ?
                React_createElement("input",{
                    type: "text",
                    className: "form-control",
                    name: "name",
                    placeholder: "Enter an approximate name, e.g. Xin Yi"
                }) :
                React_createElement("input",{
                    type: "text",
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "subject",
                defaultChecked: false,
                onChange: onRadioChange
            }),"Search By Subject"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "subject") ?
                React_createElement("select",{
                    className: "form-control",
                    name: "id"
                },__map(auxiliary.subjectInfo.data,function(subject) {
                    return React_createElement("option",{
                        value: subject.id,
                        key: subject.id
                    },subject.name," (",(__map(subject.level,function(l) {
                        return ("Year " + l);
                    })).join(", "),")");
                })) :
                React_createElement("select",{
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "cca",
                defaultChecked: false,
                onChange: onRadioChange
            }),"Search By CCA"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "cca") ?
                React_createElement("select",{
                    className: "form-control",
                    name: "id"
                },__map(auxiliary.ccaInfo.data,function(cca) {
                    return React_createElement("option",{
                        value: cca.id,
                        key: cca.id
                    },cca.name," (",cca.category,")");
                })) :
                React_createElement("select",{
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "radio form-group"
            },React_createElement("label",{
                className: "col-sm-3 col-md-2 col-lg-2 control-label"
            },React_createElement("input",{
                type: "radio",
                name: "searchby",
                value: "teacher",
                defaultChecked: false,
                onChange: onRadioChange
            }),"Search By Witness"),React_createElement("div",{
                className: "col-sm-9 col-md-7 col-lg-6"
            },((this.state.selected === "teacher") ?
                React_createElement("select",{
                    className: "form-control",
                    name: "id"
                },__map(auxiliary.teacherInfo.data,function(teacher) {
                    return React_createElement("option",{
                        value: teacher.id,
                        key: teacher.id
                    },teacher.name," (",teacher.witness_name,")");
                })) :
                React_createElement("select",{
                    className: "form-control",
                    disabled: true
                })))),React_createElement("div",{
                className: "form-group"
            },React_createElement("div",{
                className: offsetClassName
            },React_createElement("div",{
                className: "checkbox"
            },React_createElement("label",{},React_createElement("input",{
                type: "checkbox",
                defaultChecked: true,
                onChange: onCheckClick
            }," Hide Students Without Witness"))))),React_createElement("div",{
                className: "form-group"
            },React_createElement("div",{
                className: offsetClassName
            },React_createElement("button",{
                type: "submit",
                className: "btn btn-primary",
                onClick: onViewButtonClick
            },"View")))),React_createElement("div",{
                className: "row"
            },React_createElement(EntityPage,{
                wsUrl: ("/api/students?" + this.state.queryString),
                auxiliary: auxiliary,
                customButtons: customButtons,
                customFilter: customFilter
            })));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        AdminStudentsR: "displayName"
    })));
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
            onlyAdmin: true,
            component: AdminCcasR,
            dataSpec: {
                humanName: "CCA",
                humanNamePlural: "CCAs",
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
            onlyAdmin: true,
            component: AdminSubjectsR,
            dataSpec: {
                humanName: "Subject",
                humanNamePlural: "Subjects",
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
                                return React.render(React_createElement(SubmissionCompleteModal,{
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
                                    className: "btn btn-danger btn-xs",
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
                                className: "btn btn-danger btn-xs",
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
                    ((identPriv === "PrivAdmin") ?
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
                src: "/static/res/rv.png",
                style: {
                    height: "1em",
                    position: "relative",
                    top: "-0.2em",
                    margin: "0 0.3em 0 0"
                }
            }),"RVHS Science Lab Undertaking — For Teachers")),React_createElement("p",{},(("You are logged in as " + identUser) + ". "),((identPriv === "PrivAdmin") ?
                "You are an administrator. " :
                null),React_createElement("a",{
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
            return ((typeof(window.WebSocket) === "undefined") ?
                React.render(React_createElement(Modal,{
                    canClose: false,
                    title: "Browser Unsupported"
                },React_createElement("p",{},"Your browser is too old to use this website. This website requires at least Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available.")),getModalWrapper()) :
                React.render(React_createElement((pageSpec[pathname]).component,{}),($("#main-content")).get(0)));
        }
    },{
        render: function() {
            return false;
        }
    },_.invert({
        Page: "displayName"
    })));
    return React.render(React_createElement(Page,{}),document.body);
});


// Local Variables:
// eval: (add-hook (quote after-save-hook) (lambda nil (shell-command "es6c -w admin.js > admin.es5.js") (shell-command "es6c admin.js > ../static/admin.min.js")) nil t)
// End:
