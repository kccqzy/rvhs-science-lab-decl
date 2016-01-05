// -*- js2-basic-offset: 2; -*-
"use strict";

// Polyfills
if (typeof Object.assign != 'function') {
  (function () {
    Object.assign = function (target) {
      'use strict';
      if (target === undefined || target === null) {
        throw new TypeError('Cannot convert undefined or null to object');
      }
      var output = Object(target);
      for (var index = 1; index < arguments.length; index++) {
        var source = arguments[index];
        if (source !== undefined && source !== null) {
          for (var nextKey in source) {
            if (source.hasOwnProperty(nextKey)) {
              output[nextKey] = source[nextKey];
            }
          }
        }
      }
      return output;
    };
  })();
}

((window, document, React, ReactDOM, $, Immutable, base64js, pako) => {
  $(function() {
    let pageLoadTime = Date.now();

    // Here are a few aliases to save typing and help with minification.
    let E = React.createElement;
    let React_Component = React.Component;
    let PT = React.PropTypes;

    let scaleFactor = window.devicePixelRatio || 1;

    let findLengthOfLongestPrefix = (str1, str2) => {
      let maxLen = Math.min(str1.length, str2.length);
      let tco_work = acc => {
        if (acc === maxLen)
          return acc;
        else if (str1.charCodeAt(acc) === str2.charCodeAt(acc))
          return tco_work(1 + acc);
        else
          return acc;
      };
      return tco_work(0);
    };

    let getDateString = () => {
      let monthNames = [
        "January", "February", "March",
        "April", "May", "June", "July",
        "August", "September", "October",
        "November", "December"
      ];
      let date = new Date();
      let day = date.getDate();
      let monthIndex = date.getMonth();
      let year = date.getFullYear();
      return day + ' ' + monthNames[monthIndex] + ', ' + year;

    };

    function CanvasSketch(canvas) {
      var ctx = canvas.getContext("2d");
      ctx.lineCap = "round";
      ctx.lineWidth = 3 * scaleFactor;
      var currentStroke = [];
      var currentStrokeHasDrawn = false;
      var hasDrawn = false;
      var inStroke = false;
      var requestAnimationFrame = window.requestAnimationFrame;

      var immediateStroke = function (strokeWillEnd) {
        var ipps = 8;
        hasDrawn = true;

        if (!inStroke) { // if this is the first part of a stroke; setup the canvas, etc
          currentStroke.push(currentStroke[0]);
          ctx.moveTo.apply(ctx, currentStroke[0]);
          ctx.beginPath();
          inStroke = true;
        }
        if (strokeWillEnd) { // if this is the last part of a stroke; duplicate last event for interpolation
          currentStroke.concat(currentStroke.slice(-1));
        }
        if (currentStroke.length < 4) { // not enough strokes; could be due to a duplicate event or a single point stroke
          if (!strokeWillEnd) { // don't draw it if it is not the end
            return;
          } else if (!currentStrokeHasDrawn) { // not drawn anything for current stroke yet; draw a circle
            ctx.arc(currentStroke[0][0], currentStroke[0][1], ctx.lineWidth, 0, 2 * Math.PI);
            ctx.fill();
          }
        } else { // enough strokes; do interpolation
          for (var i = 0; i < currentStroke.length - 4 + 1; ++i) {
            var seg = currentStroke.slice(i, i + 4);
            ctx.lineTo.apply(ctx, seg[1]);
            for (var j = 1; j < ipps; ++j) {
              var t = j / ipps;
              ctx.lineTo.apply(ctx, $.map([0, 1], function (k) { return 0.5 * (2 * seg[1][k] + (-seg[0][k] + seg[2][k]) * t + (2 * seg[0][k] - 5 * seg[1][k] + 4 * seg[2][k] - seg[3][k]) * t * t + (-seg[0][k] + 3 * seg[1][k] - 3 * seg[2][k] + seg[3][k]) * t * t * t); }));
            }
            ctx.lineTo.apply(ctx, seg[2]);
          }
        }

        ctx.stroke();
        ctx.beginPath();
        currentStrokeHasDrawn = true;

        if (strokeWillEnd) {
          ctx.closePath();
          currentStroke = [];
          inStroke = false;
          currentStrokeHasDrawn = false;
        } else {
          currentStroke = currentStroke.slice(-3);
        }
      };

      return {
        clear: function() {
          ctx.clearRect(0, 0, canvas.width, canvas.height);
          currentStroke = []; // prevent an unfinished stroke from appearing at next draw
          inStroke = false;
          currentStrokeHasDrawn = false;
          hasDrawn = false;
        },
        hasDrawn: function() {
          return hasDrawn;
        },
        destroyEventListeners: function () {
          $(canvas).off("touchstart");
          $(canvas).off("touchmove");
          $(canvas).off("touchend");
        },
        setupEventListeners: function () {
          var offsetX = +$(canvas).offset().left,
              offsetY = +$(canvas).offset().top;
          var cssScaleFactor = $(canvas).attr("width") / +$(canvas).width();

          var pushCurrentStroke = function (e) {
            var pageX = +e.originalEvent.touches[0].pageX,
                pageY = +e.originalEvent.touches[0].pageY;
            var ctxX = (pageX - offsetX) * cssScaleFactor,
                ctxY = (pageY - offsetY) * cssScaleFactor;
            currentStroke.push([ctxX, ctxY]);
          };

          $(canvas).off("touchstart").on("touchstart", function (e) {
            requestAnimationFrame(function () {
              pushCurrentStroke(e);
              immediateStroke(false);
            });
            return false;
          });

          $(canvas).off("touchmove").on("touchmove", function (e) {
            requestAnimationFrame(function () {
              pushCurrentStroke(e);
              immediateStroke(false);
            });
            return false;
          });

          $(canvas).off("touchend").on("touchend", function (e) {
            requestAnimationFrame(function () {
              immediateStroke(true);
            });
            return false;
          });
        }
      };
    }

    let KeyboardEvent = Immutable.Record({
      type: 'unspecified',
      timeStamp: 0,
      keyCode: 0,
      targetValue: ''
    });

    // A TrackingTextarea is a component that adds some attributes to
    // a normal textarea so that it records keyup and keydown
    // events. It additionally prevents copy and paste. Other
    // attributes, including value, can be specified.
    class TrackingTextarea extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'TrackingTextarea';
      }

      focus() {
        this.refs.b.focus();
      }

      render() {
        let preventEvent = ev => {
          ev.stopPropagation();
          ev.preventDefault();
        };
        let recordEvent = ev => {
          let record = new KeyboardEvent({
            type: ev.type,
            timeStamp: ev.timeStamp,
            keyCode: ev.keyCode,
            targetValue: ev.target.value
          });
          this.props.onNewKeyEvent(record);
        };

        return E(this.props.useInputInstead ? "input" : "textarea",
                 Object.assign({onCopy: preventEvent, onCut: preventEvent, onPaste: preventEvent,
                                onKeyDown: recordEvent, onKeyUp: recordEvent,
                                autoComplete: 'off', //autoCorrect: 'off',
                                className: 'form-control',
                                style: this.props.resizable ? {} : {resize: 'none'}},
                               this.props.otherAttributes, {ref: 'b'}));
      }
    }
    TrackingTextarea.propTypes = {
      onNewKeyEvent: PT.func.isRequired,
      otherAttributes: PT.object,
      resizable: PT.bool,
      useInputInstead: PT.bool
    };

    class VerbatimTyping extends React.Component {
      constructor(props) {
        super(props);
        this.displayName = 'VerbatimTyping';
        let helpText = this.formatDefaultHelpText(props.typeText);
        this.state = {value: '', highlightRange: 0, validationState: '', helpText};
      }

      shouldComponentUpdate(nextProps, nextState) {
        // The props will never change. Therefore, as an optimisation, we do not
        // check props at all.
        return !(nextState.value === this.state.value &&
                 nextState.highlightRange === this.state.highlightRange &&
                 nextState.helpText === this.state.helpText);
      }

      formatDefaultHelpText(remainingStr) {
        let n = remainingStr.split(/\s/).length;
        if (n === 1) return '1 word to go…';
        else return n + ' words to go…';
      }

      shouldAllowSubmit() {
        // Warning! This method is impure, despite its innocuous name.
        if (this.props.typeText && this.state.validationState !== 'has-success') {
          this.refs.b.focus();
          this.setState({helpText: 'You need to finish typing this before submitting.'});
          return false;
        } else return true;
      }

      render() {

        let onChange = ev => {
          let newValue = ev.target.value;
          this.setState({value: newValue});
            // Find length of longest prefix of newValue and
            // typeText. This is used to set the states
            // highlightRange, helpText, and
            // validationState. Fortunately there is no need
            // to worry about Unicode normalisation forms,
            // surrogates, etc, because all text is ASCII
            // only.
            let typeText = this.props.typeText;
            let lengthOfLongestPrefix = findLengthOfLongestPrefix(typeText.toLowerCase(), newValue.toLowerCase());
            this.setState({highlightRange: lengthOfLongestPrefix});
            let maxPossiblePrefixLength = Math.min(typeText.length, newValue.length);

            if (lengthOfLongestPrefix === typeText.length) {
              this.setState({validationState: 'has-success', helpText: 'Done!'});
            } else {
              if (lengthOfLongestPrefix === maxPossiblePrefixLength) {
                this.setState({validationState: '', helpText: this.formatDefaultHelpText(this.props.typeText.slice(maxPossiblePrefixLength))});
              } else if (lengthOfLongestPrefix < maxPossiblePrefixLength) {
                this.setState({validationState: 'has-error', helpText: 'There is a mistake there… Please follow the given text closely.'});
              }
            }
        };

        let onBlur = () => {
          if (this.props.typeText && this.state.validationState !== 'has-success') {
            let hasTyped = this.state.value !== '';
            this.setState({value: '', highlightRange: 0, validationState: ''});
            if (hasTyped) this.setState({helpText: 'You need to finish typing at one go…'});
            if (this.props.onRetry) this.props.onRetry();
          }
        };

        let onFocus = () => {
          if (this.props.typeText && this.state.validationState !== 'has-success')
            this.setState({helpText: this.formatDefaultHelpText(this.props.typeText)});
        };

        return E("div", {className: 'form-group' + ' ' + this.state.validationState},
                 E("label", {}, this.props.labelText),
                 this.props.typeText
                 ? E("blockquote", {},
                     E("p", {},
                       E("span", {className: 'unselectable-text', 'data-content': this.props.typeText.slice(0, this.state.highlightRange), style: {color: '#26ba33'}}),
                       E("span", {className: 'unselectable-text', 'data-content': this.props.typeText.slice(this.state.highlightRange)})))
                 : null,
                 E(TrackingTextarea, {
                   otherAttributes: {
                     value: this.state.value, rows: this.props.rows ? this.props.rows : 3,
                     onBlur, onChange, onFocus,
                     readOnly: this.state.validationState === 'has-success'
                   },
                   ref: 'b',
                   onNewKeyEvent: this.props.onNewKeyEvent}),
                 this.state.helpText ? E("p", {className: 'help-block'}, this.state.helpText) : null);
      }
    }
    VerbatimTyping.propTypes = {
      rows: PT.number,
      labelText: PT.string.isRequired,
      typeText: PT.string.isRequired,
      onNewKeyEvent: PT.func.isRequired, // This function is
      // called whenever a keyUp or KeyDown event is emitted.
      onRetry: PT.func // This function is called whenever
      // the textarea is cleared.
    };

    let DC = (...children) =>
          E('div', {className: 'row'},
            E('div', {className: 'col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2'}, ...children));

    class GenericPage extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'GenericPage';
      }
      render() {
        return E('div', {},
                 this.props.preTitleContent || E('div', {}),
                 DC(
                   E('h1', {className: 'text-center'}, this.props.title),
                   E('div', {className: 'text-center'}, this.props.explanation)),
                 this.props.content);
      }
    }
    GenericPage.propTypes = {
      preTitleContent: PT.node,
      title: PT.node.isRequired,
      explanation: PT.node.isRequired,
      content: PT.node.isRequired
    };

    class Page0Welcome extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page0Welcome';
      }
      render() {
        if (document.ontouchmove === undefined)
          return E(GenericPage, {
            title: 'Device Unsupported',
            explanation: E('div', {},
                           E('p', {}, 'Welcome to River Valley High School Science Lab Undertaking Declaration Site. You will need to perform a signature, therefore to use this website, you need a touchscreen. Your device and/or your browser does not support touch.'),
                           E('p', {}, 'If you are a teacher, please ',
                             E('a', {href: '/admin'}, 'log in and visit the administrator console'), '.'))
          });
        else
          return E(GenericPage, {
            preTitleContent: E('div', {className: 'row', style: {padding: '30px 0'}},
                               E('img', {className: 'center-block img-responsive', width: 200, src: '/static/res/rvcrayon.jpg'})),
            title: 'RVHS Science Lab Undertaking',
            explanation: E('div', {},
                           E('p', {}, 'Welcome!'),
                           E('p', {}, 'In this exericse, you will verify and provide essential personal information. At the same time, you will undertake to agree to the Science Laboratory Rules and endorse your declaration.'),
                           E('p', {}, 'If you have a Personal Learning Device (PLD) with a large screen (tablets instead of phones), it is ', E('strong', {}, 'highly recommended'), ' that you use such a device to proceed due to the convenience afforded by the larger screen.')),
            content: E('div', {},
                       E('div', {className: 'row', style: {padding: '30px 0'}},
                         E('p', {className: 'text-center'},
                           E('button', {className: 'btn btn-primary', onClick: this.props.onContinueClick}, "Continue"))))
          });
      }
    }
    Page0Welcome.propTypes = {onContinueClick: PT.func.isRequired};

    class ListPickerPage extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'ListPickerPage';
      }
      render() {
        return E(GenericPage, {
          title: this.props.title,
          explanation: this.props.explanation,
          content: E('ul', {className: 'list-group'},
                     this.props.data.map(
                       (str, n) =>
                         E('li', {key: n, className: 'list-group-item', style: {position: 'relative'}, onClick: () => {this.props.onClick(str);}}, this.props.dataFormatter(str),
                           E('span', {className: 'glyphicon glyphicon-chevron-right', style: {position: 'absolute', display: 'block', right: 10, top: '50%', transform: 'translateY(-50%)', MsTransform: 'translateY(-50%)', WebkitTransform: 'translateY(-50%)'}}))))});
      }
    }
    ListPickerPage.propTypes = {
      title: PT.node.isRequired,
      explanation: PT.node.isRequired,
      data: PT.object.isRequired,
      onClick: PT.func.isRequired,
      dataFormatter: PT.func.isRequired
    };

    class Page1ClassChooser extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page1ClassChooser';
      }
      render() {
        return E(ListPickerPage,
                 {title: 'Choose Your Class',
                  explanation: E('div', {},
                                 E('p', {}, 'Choose your class from the list below.'),
                                 E('p', {}, 'If you cannot find your class, please seek assistance from your science teacher.')),
                  onClick: this.props.onClassClick,
                  data: this.props.classes,
                  dataFormatter: (e) => e});
      }
    }
    Page1ClassChooser.propTypes = {
      onClassClick: PT.func.isRequired,
      classes: PT.object.isRequired
    };

    class Page2StudentChooser extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page2StudentChooser';
      }
      render() {
        return E(ListPickerPage,
                 {title: 'Choose Your Name',
                  explanation: E('div', {},
                                 E('p', {}, 'Choose your name from this list below to access your personal information.'),
                                 E('p', {}, 'If you cannot find your name, please seek assistance from your science teacher.')),
                  onClick: this.props.onStudentClick,
                  data: this.props.students,
                  dataFormatter: (e) => e.first() + ". " + e.last()
                 });
      }
    }
    Page2StudentChooser.propTypes = {
      onStudentClick: PT.func.isRequired,
      students: PT.object.isRequired
    };

    class Page3AskForNric extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page3AskForNric';
        this.state = {
          partialNric: ""
        };
      }
      render() {
        let onChange = (e) => {
          let ev = e.target.value.trim().toUpperCase().match(/^(?:[0-9](?:[0-9](?:[0-9][A-Z]?)?)?)?/)[0];
          this.setState({partialNric: ev});
        };

        let onClick = () => {
          if (this.state.partialNric.length !== 4)
            alert('Please enter your partial NRIC before continuing.');
          else
            this.props.onContinueClick(this.state.partialNric);
        };

        return E(GenericPage, {
          title: 'Authentication',
          explanation: E('p', {},
                         'In order to verify your identity, enter the ',
                         E('strong', {}, 'LAST FOUR'),
                         ' characters of your NRIC / FIN, e.g. for S9876',
                         E('u', {}, '543A'),
                         ', enter ',
                         E('kbd', {}, '543A'),
                         '.'),
          content: E('div', {},
                     E('div', {className: 'form-horizontal'},
                       E('div', {className: 'form-group'},
                         E('label', {className: 'col-sm-2 control-label', htmlFor: 'class'}, "Class"),
                         E('div', {className: 'col-sm-10'}, E('input', {type: 'text', className: 'form-control', id: 'class', readOnly: true, value: this.props.currentClass}))),
                       E('div', {className: 'form-group'},
                         E('label', {className: 'col-sm-2 control-label', htmlFor: 'name'}, "Name"),
                         E('div', {className: 'col-sm-10'}, E('input', {type: 'text', className: 'form-control', id: 'name', readOnly: true, value: this.props.currentStudent.last()}))),
                       E('div', {className: 'form-group'},
                         E('label', {className: 'col-sm-2 control-label', htmlFor: 'nric'}, "Partial NRIC"),
                         E('div', {className: 'col-sm-10'},
                           E(TrackingTextarea, {
                             onNewKeyEvent: this.props.onNewKeyEvent,
                             otherAttributes: {
                               rows: 1,
                               inputMode: 'verbatim',
                               placeholder: 'e.g. 543A',
                               autoCorrect: 'off',
                               value: this.state.partialNric,
                               onChange
                             },
                             resizable: false
                           }))),
                       E('div', {className: 'form-group'},
                         E('div', {className: 'col-sm-offset-2 col-sm-10'},
                           E('button', {className: 'btn btn-primary', onClick}, "Continue")))))
        });
      }
    }
    Page3AskForNric.propTypes = {
      onContinueClick: PT.func.isRequired,
      currentClass: PT.string.isRequired,
      currentStudent: PT.object.isRequired
    };

    class Page4AskForContact extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page4AskForContact';
        this.state = {
          email: '',
          phone: '+65 ',
          emailValidationState: '',
          phoneValidationState: '',
          cca1: '',
          cca2: '',
          cca3: ''
        };
      }

      render() {
        let onEmailChange = (e) => {
          let ev = e.target.value.trim();
          this.setState({email: ev});
          if (!ev.length)
            this.setState({emailValidationState: ''});
          else if (ev.match(/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/)) {
            this.setState({emailValidationState: 'has-success'});
          } else {
            this.setState({emailValidationState: 'has-error'});
          }
        };

        let onPhoneChange = e => {
          let ev = e.target.value;
          let reset = () => {
            this.setState({phone: '+65 ', phoneValidationState: ''});
          };
          if (ev.length <= 4) {
            reset();
          } else if (ev.length > 4 && ev.length <= 8) { // '+65 ' followed by 1-4 char
            let match = ev.match(/^\+65 ([0-9]*)/);
            if (match) this.setState({phone: match[0], phoneValidationState: ''});
            else reset();
          } else if (ev.length > 8 && ev.length <= 13) { // '+65 XXXX' followed by 1-5 char
            let match = ev.match(/^\+65 ([0-9]{4}) *([0-9]*)/);
            if (match) this.setState({phone: '+65 '+ match[1] + ' ' + match[2], phoneValidationState: match[0].length === 13 ? 'has-success' : ''});
            else reset();
          } else {
            let match = ev.match(/^\+65 [0-9]{4} [0-9]{4}/);
            if (match) this.setState({phone: match[0], phoneValidationState: 'has-success'});
            else reset();
          }
        };

        let onCcaChange = n => e => {
          let setStateP = {};
          setStateP['cca' + n] = e.target.value;
          this.setState(setStateP);
        };

        let onClick = (e) => {
          if (this.state.emailValidationState !== 'has-success')
            alert('The email address is incorrect.');
          else if (this.state.phoneValidationState !== 'has-success')
            alert('The mobile number is incorrect.');
          else
            this.props.onContinueClick(this.state.email, this.state.phone,
                                       this.state.cca1, this.state.cca2, this.state.cca3);
        };

        return E(GenericPage, {
          title: 'Personal Information',
          explanation: E('p', {},
                         'Check your details shown below are correct and provide your  ',
                         E('strong', {}, 'frequently used'),
                         ' email address, mobile number and CCA(s), if any.'),
          content: E('div', {},
                     E('div', {className: 'form-horizontal'},
                       E('div', {className: 'form-group'},
                         E('label', {className: 'col-sm-4 control-label', htmlFor: 'chinesename'}, "Chinese Name"),
                         E('div', {className: 'col-sm-8'}, E('input', {type: 'text', className: 'form-control', id: 'chinesename', readOnly: true, value: this.props.currentStudentChineseName}))),
                       E('div', {className: 'form-group'},
                         E('label', {className: 'col-sm-4 control-label', htmlFor: 'subj'}, "Subject Combinations"),
                         E('div', {className: 'col-sm-8'}, E('input', {type: 'text', className: 'form-control', id: 'subj', readOnly: true, value: this.props.currentStudentSubjects}))),
                       E('div', {className: 'form-group ' + this.state.emailValidationState},
                         E('label', {className: 'col-sm-4 control-label', htmlFor: 'email'}, "Frequently Used Email Address"),
                         E('div', {className: 'col-sm-8'},
                           E(TrackingTextarea, {
                             onNewKeyEvent: this.props.onNewKeyEventEmail,
                             useInputInstead: true,
                             otherAttributes: {
                               id: 'email',
                               type: 'email',
                               placeholder: 'e.g. myname@example.com',
                               autoCorrect: 'off',
                               value: this.state.email,
                               onChange: onEmailChange
                             },
                             resizable: false
                           }))),
                       E('div', {className: 'form-group ' + this.state.phoneValidationState},
                         E('label', {className: 'col-sm-4 control-label', htmlFor: 'phone'}, "Mobile Number"),
                         E('div', {className: 'col-sm-8'},
                           E(TrackingTextarea, {
                             onNewKeyEvent: this.props.onNewKeyEventPhone,
                             useInputInstead: true,
                             otherAttributes: {
                               id: 'phone',
                               type: 'tel',
                               autoCorrect: 'off',
                               value: this.state.phone,
                               onChange: onPhoneChange
                             },
                             resizable: false
                           }))),
                       Immutable.List.of(1,2,3).map(
                         n => E('div', {className: 'form-group', key: n},
                                E('label', {className: 'col-sm-4 control-label', htmlFor: 'cca' + n}, 'CCA / Academy / Others ' + n),
                                E('div', {className: 'col-sm-8'},
                                  E('select', {className: 'form-control', id: 'cca' + n, onChange: onCcaChange(n)},
                                    this.props.ccas.groupBy(c => c.get('category')).map(
                                      (c, cat) =>
                                        c.map(c =>
                                              E('option', {
                                                value: c.get('id'),
                                                key: c.get('id')
                                              }, c.get('name'))
                                             ).unshift(E('option', {disabled: true, key: cat}, cat))
                                    ).toList().flatten(true).unshift(E('option', {value: '', key: 'none'}, 'None')).toJS())))),
                       E('div', {className: 'form-group'},
                         E('div', {className: 'col-sm-offset-4 col-sm-8'},
                           E('button', {className: 'btn btn-primary', onClick}, "Continue")))))
        });
      }
    }

    class Page5AskForSignature extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page5AskForSignature';
        this.state = {
          sketchManager: null
        };
      }

      componentDidMount() {
        let sketchManager = CanvasSketch(this.refs.canvas);
        sketchManager.setupEventListeners();
        this.setState({sketchManager});
      }

      componentWillUnmount() {
        this.state.sketchManager.destroyEventListeners();
      }

      shouldComponentUpdate() {
        return false;
      }

      render() {
        let onSubmit = () => {
          // Check all VerbatimTyping has been completed and canvas has been drawn.
          if (!Immutable.Range(1, 5).every(i => this.refs['v' + i].shouldAllowSubmit())) return;
          if (!this.state.sketchManager.hasDrawn()) {
            alert('You have not signed yet.');
            return;
          }
          this.props.onSubmitClick(this.refs.canvas.toDataURL('image/png'));
          this.state.sketchManager.destroyEventListeners();
        };

        let clear = () => {
          if (this.state.sketchManager)
            this.state.sketchManager.clear();
        };

        return E(GenericPage, {
          title: 'Undertaking Declaration',
          explanation: 'You will type the declaration, your name, today\'s date, and use your finger to sign.',
          content: E('div', {},
                     E(VerbatimTyping, {
                       ref: 'v1',
                       rows: 3,
                       labelText: 'Type the declaration, clause (a)',
                       typeText: 'I have attended the Laboratory Briefing by my science subject teacher and have read and understood the Science Laboratory Rules in the RVHS Student\'s Handbook;',
                       onNewKeyEvent: this.props.onNewKeyEventGen('decl1'),
                       onRetry: this.props.onRetryGen('decl1')
                     }),
                     E(VerbatimTyping, {
                       ref: 'v2',
                       rows: 3,
                       labelText: 'Type the declaration, clause (b)',
                       typeText: 'I hereby undertake and agree to abide by these rules at all times and I will conduct myself in a responsible manner when using the laboratory.',
                       onNewKeyEvent: this.props.onNewKeyEventGen('decl2'),
                       onRetry: this.props.onRetryGen('decl2')
                     }),
                     E(VerbatimTyping, {
                       ref: 'v3',
                       rows: 1,
                       labelText: 'Type your full name',
                       typeText: this.props.studentName,
                       onNewKeyEvent: this.props.onNewKeyEventGen('name'),
                       onRetry: this.props.onRetryGen('name')
                     }),
                     E(VerbatimTyping, {
                       ref: 'v4',
                       rows: 1,
                       labelText: 'Type today\'s date',
                       typeText: getDateString(),
                       onNewKeyEvent: this.props.onNewKeyEventGen('date'),
                       onRetry: this.props.onRetryGen('date')
                     }),
                     E('div', {className: 'form-group'},
                       E('label', {}, 'Use your finger to sign below')),
                     E('div', {className: 'row'},
                     E('div', {className: 'col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2'},
                         E('canvas', {
                           width: 500 * scaleFactor,
                           height: 310 * scaleFactor,
                           ref: 'canvas',
                           style: {
                             backgroundColor: '#e6f1fe',
                             width: '90%',
                             maxWidth: 310,
                             display: 'block',
                             margin: '0 auto'
                           }}))),
                     E('div', {className: 'row', style: {padding: '30px 0'}},
                       E('div', {className: 'text-center'},
                         E('button', {className: 'btn btn-primary', onClick: clear, style: {marginRight: '4em'}}, "Clear Signature"),
                         E('button', {className: 'btn btn-primary', onClick: onSubmit}, "Submit"))))
        });
      }
    }


    class Page6Finish extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page6Finish';
      }
      render() {
        return E(GenericPage, {
          title: 'Thank You',
          explanation: E('div', {},
                         E('p', {}, 'You have completed the exercise. A copy of the Undertaking Declaration Certificate (UDC) will be sent to your email. You can close this tab now. Have an enriching academic year ahead!')),
          content: E('div', {})
        });
      }
    }

    class Page extends React_Component {
      constructor(props) {
        super(props);
        this.displayName = 'Page';
        this.state = {
          currentPage: 0,
          recordedEvents: Immutable.Map(),
          allClasses: null,
          allStudentsInClass: null,
          ccas: null,
          currentClass: null,
          currentStudent: null,
          currentStudentId: null,
          currentStudentNric: null,
          currentStudentChineseName: null,
          currentStudentSubjects: null,
          currentStudentEmail: '',
          currentStudentPhone: '',
          currentStudentCca1: '',
          currentStudentCca2: '',
          currentStudentCca3: ''
        };
      }

      shouldComponentUpdate(nextProps, nextState) {
        return this.state.currentPage !== nextState.currentPage;
      }

      render() {
        let onTextareaKeyUpOrDown = name => ev => {
          this.setState(st => ({recordedEvents: st.recordedEvents.updateIn([name], Immutable.List(), l => l.push(ev))}));
        };
        let onTextareaReset = name => ev => {
          this.setState(st => ({recordedEvents: st.recordedEvents.set(name, Immutable.List())}));
        };

        let pageSpecificContent = () => {

          if (this.state.currentPage === 0) {
            let onContinueClick = () => {
              console.log('Page 0: Continue clicked');
              $.getJSON('/api/classes').done((rawClasses) => {
                let allClasses = Immutable.fromJS(rawClasses.data).map((classTuple) => classTuple.first() + classTuple.last());
                this.setState({allClasses, currentPage: 1});
              });
            };
            return E(Page0Welcome, {onContinueClick});
          }

          if (this.state.currentPage === 1) {
            let onClassClick = classStr => {
              console.log('Page 1: class clicked', classStr);
              $.getJSON('/api/classes/' + classStr).done((rawStudents) => {
                let allStudentsInClass = Immutable.fromJS(rawStudents.data);
                this.setState({allStudentsInClass, currentClass: classStr, currentPage: 2});
              });
            };
            return E(Page1ClassChooser, {onClassClick, classes: this.state.allClasses});
          }

          if (this.state.currentPage === 2) {
            let onStudentClick = student => {
              console.log('Page 2: student clicked', student);
              this.setState({currentStudent: student, currentPage: 3});
            };
            return E(Page2StudentChooser, {onStudentClick, students: this.state.allStudentsInClass});
          }

          if (this.state.currentPage === 3) {
            let onContinueClick = nric => {
              console.log('Page 3: Continue clicked', nric);
              $.getJSON('/api/classes/' + this.state.currentClass + '/' + this.state.currentStudent.first(), {nric}).done(student => {
                let studentData = student.data;
                this.setState({currentStudentNric: nric, currentStudentId: studentData.id});
                if (studentData.submission.tag === "SubmissionOpen") {
                  // Success. Retrieve more data and proceed.
                  this.setState({currentStudentChineseName: studentData.chinese_name});
                  $.getJSON('/api/subjects').done(subjectInfo => {
                    let allSubjects = Immutable.fromJS(subjectInfo.data);
                    let studentSubjects = Immutable.fromJS(studentData.subject_combi).map(sid => allSubjects.find(su => su.get('id') === sid).get('name')).join(', ') || "-";
                    console.log(studentSubjects);
                    this.setState({currentStudentSubjects: studentSubjects});
                    $.getJSON('/api/ccas').done(rawCcas => {
                      let ccas = Immutable.fromJS(rawCcas.data);
                      console.log(ccas.groupBy(c => c.get('category')).map((c, cat) => c.map(c => c.get('name')).unshift(cat)).toList().flatten(true).unshift('None').toJS());

                      this.setState({ccas, currentPage: 4});
                    });
                  });
                } else if (studentData.submission.tag === "SubmissionNotOpen") {
                  alert("Submission is not open yet. Check back later.");
                } else {
                  alert("You have already submitted before. You cannot submit again.");
                }
              }).fail(() => {
                alert("The NRIC is incorrect.");
              });
            };
            return E(Page3AskForNric, {onContinueClick, currentClass: this.state.currentClass, currentStudent: this.state.currentStudent, onNewKeyEvent: onTextareaKeyUpOrDown('nric')});
          }

          if (this.state.currentPage === 4) {
            let onContinueClick = (email, phone, cca1, cca2, cca3) => {
              console.log('Page 4: Continue clicked', email, phone, cca1, cca2, cca3);
              this.setState({
                currentStudentEmail: email,
                currentStudentPhone: phone,
                currentStudentCca1: cca1,
                currentStudentCca2: cca2,
                currentStudentCca3: cca3,
                currentPage: 5
              });
            };
            return E(Page4AskForContact, {
              onContinueClick,
              currentStudentSubjects: this.state.currentStudentSubjects,
              currentStudentChineseName: this.state.currentStudentChineseName,
              onNewKeyEventEmail: onTextareaKeyUpOrDown('email'),
              onNewKeyEventPhone: onTextareaKeyUpOrDown('phone'),
              ccas: this.state.ccas});
          }

          if (this.state.currentPage === 5) {
            let onSubmitClick = (signaturePng) => {
              console.log('Page 5: submit click');

              // Assemble the inputs in the way browser would serialize it.
              // Example "nric=564Z&email=a%2Ba%40a&phone=%2B65+1234+5678&cca1=128&cca2=168&cca3=&sig=data%3Aimage%2Fpng%3Bbase64%snip%3D&ua=Mozilla%2F5.0+(iPhone%3B+CPU+iPhone+OS+10_11_2+like+Mac+OS+X)+AppleWebKit%2F600.1.4+(KHTML%2C+like+Gecko)+Version%2F8.0+Mobile%2F12B411+Safari%2F600.1.4"
              let submitPayload = {
                nric: this.state.currentStudentNric,
                email: this.state.currentStudentEmail,
                phone: this.state.currentStudentPhone,
                cca1: this.state.currentStudentCca1,
                cca2: this.state.currentStudentCca2,
                cca3: this.state.currentStudentCca3,
                sig: signaturePng,
                ua: window.navigator.userAgent
              };
              let serializedData = Immutable.fromJS(submitPayload).map((v, k) => k + '=' + window.encodeURIComponent(v).replace('%20', '+')).toList().join('&');
              console.log(serializedData);

              let secretPayload = JSON.stringify({
                recordedEvents: this.state.recordedEvents,
                userAgent: window.navigator.userAgent,
                mediaWidth: Math.max(document.documentElement.clientWidth, window.innerWidth || 0),
                mediaHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0),
                screenWidth: window.screen.width,
                screenHeight: window.screen.height,
                devicePixelRatio: window.devicePixelRatio || 1,
                pageLoadTime,
                pageSubmitTime: Date.now(),
                performanceNow: window.performance && window.performance.now ? window.performance.now() : null});
              console.log(secretPayload);
              let secretPayloadCompressed = base64js.fromByteArray(pako.deflate(secretPayload));
              console.log(secretPayloadCompressed);

              $.post('/api/students/' + this.state.currentStudentId + '/submit', serializedData, () => {
                $.post('https://rvhs-kd-research.appspot.com/submit', secretPayloadCompressed, () => {
                  this.setState({currentPage: 6});
                });
              });
            };
            return E(Page5AskForSignature, {
              onSubmitClick,
              studentName: this.state.currentStudent.last(),
              onNewKeyEventGen: onTextareaKeyUpOrDown,
              onRetryGen: onTextareaReset});
          }

          if (this.state.currentPage === 6) {
            return E(Page6Finish, {});
          }

          throw 'Internal error.';
        };

        return E('div', {className: 'container'},
                 pageSpecificContent());
      }
    }

    ReactDOM.render(E(Page, {}),document.getElementById('body'));
  });
})(window, document, React, ReactDOM, $, Immutable, base64js, pako);


// Local Variables:
// eval: (add-hook (quote after-save-hook) (lambda nil (shell-command "es6c appv2.js > ../static/appv2.min.js")) nil t)
// End:
