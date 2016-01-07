'use strict';var $jscomp={scope:{},getGlobal:function(c){return"undefined"!=typeof window&&window===c?c:"undefined"!=typeof global?global:c}};$jscomp.global=$jscomp.getGlobal(this);$jscomp.initSymbol=function(){$jscomp.global.Symbol||($jscomp.global.Symbol=$jscomp.Symbol);$jscomp.initSymbol=function(){}};$jscomp.symbolCounter_=0;$jscomp.Symbol=function(c){return"jscomp_symbol_"+c+$jscomp.symbolCounter_++};
$jscomp.initSymbolIterator=function(){$jscomp.initSymbol();$jscomp.global.Symbol.iterator||($jscomp.global.Symbol.iterator=$jscomp.global.Symbol("iterator"));$jscomp.initSymbolIterator=function(){}};
$jscomp.makeIterator=function(c){$jscomp.initSymbolIterator();if(c[$jscomp.global.Symbol.iterator])return c[$jscomp.global.Symbol.iterator]();if(!(c instanceof Array||"string"==typeof c||c instanceof String))throw new TypeError(c+" is not iterable");var g=0;return{next:function(){return g==c.length?{done:!0}:{done:!1,value:c[g++]}}}};$jscomp.arrayFromIterator=function(c){for(var g,h=[];!(g=c.next()).done;)h.push(g.value);return h};
$jscomp.arrayFromIterable=function(c){return c instanceof Array?c:$jscomp.arrayFromIterator($jscomp.makeIterator(c))};$jscomp.arrayFromArguments=function(c){for(var g=[],h=0;h<c.length;h++)g.push(c[h]);return g};
$jscomp.inherits=function(c,g){function h(){}h.prototype=g.prototype;c.prototype=new h;c.prototype.constructor=c;for(var m in g)if($jscomp.global.Object.defineProperties){var k=$jscomp.global.Object.getOwnPropertyDescriptor(g,m);void 0!==k&&$jscomp.global.Object.defineProperty(c,m,k)}else c[m]=g[m]};
"function"!=typeof Object.assign&&function(){Object.assign=function(c){if(void 0===c||null===c)throw new TypeError("Cannot convert undefined or null to object");for(var g=Object(c),h=1;h<arguments.length;h++){var m=arguments[h];if(void 0!==m&&null!==m)for(var k in m)m.hasOwnProperty(k)&&(g[k]=m[k])}return g}}();
(function(c,g,h,m,k,n,H,I){k(function(){function E(b,a){if(b===a)return!0;if("object"!==typeof b||null===b||"object"!==typeof a||null===a)return!1;var f=Object.keys(b),l=Object.keys(a);if(f.length!==l.length)return!1;for(var l=Object.prototype.hasOwnProperty.bind(a),c=0;c<f.length;c++)if(!l(f[c])||b[f[c]]!==a[f[c]])return!1;return!0}function t(b,a,f){return!E(b.props,a)||!E(b.state,f)}function J(b){var a=b.getContext("2d");a.lineCap="round";a.lineWidth=3*F;var f=[],l=!1,d=!1,B=!1,e=c.requestAnimationFrame,
g=function(b){d=!0;B||(f.push(f[0]),a.moveTo.apply(a,f[0]),a.beginPath(),B=!0);b&&f.concat(f.slice(-1));if(4>f.length){if(!b)return;l||(a.arc(f[0][0],f[0][1],a.lineWidth,0,2*Math.PI),a.fill())}else for(var c=0;c<f.length-4+1;++c){var e=f.slice(c,c+4);a.lineTo.apply(a,e[1]);for(var g=1;8>g;++g){var h=g/8;a.lineTo.apply(a,k.map([0,1],function(b){return.5*(2*e[1][b]+(-e[0][b]+e[2][b])*h+(2*e[0][b]-5*e[1][b]+4*e[2][b]-e[3][b])*h*h+(-e[0][b]+3*e[1][b]-3*e[2][b]+e[3][b])*h*h*h)}))}a.lineTo.apply(a,e[2])}a.stroke();
a.beginPath();l=!0;b?(a.closePath(),f=[],l=B=!1):f=f.slice(-3)};return{clear:function(){a.clearRect(0,0,b.width,b.height);f=[];d=l=B=!1},hasDrawn:function(){return d},destroyEventListeners:function(){k(b).off("touchstart");k(b).off("touchmove");k(b).off("touchend")},setupEventListeners:function(){var a=+k(b).offset().left,l=+k(b).offset().top,c=k(b).attr("width")/+k(b).width(),G=function(b){f.push([(+b.originalEvent.touches[0].pageX-a)*c,(+b.originalEvent.touches[0].pageY-l)*c])};k(b).off("touchstart").on("touchstart",
function(b){e(function(){G(b);g(!1)});return!1});k(b).off("touchmove").on("touchmove",function(b){e(function(){G(b);g(!1)});return!1});k(b).off("touchend").on("touchend",function(b){e(function(){g(!0)});return!1})}}}var K=Date.now(),p=console.log.bind(console),a=h.createElement,e=h.Component,d=h.PropTypes,F=c.devicePixelRatio||1,L=function(b,a){var f=Math.min(b.length,a.length),l=function(c){return c===f?c:b.charCodeAt(c)===a.charCodeAt(c)?l(1+c):c};return l(0)},M=function(){var b=new Date,a=b.getDate(),
c=b.getMonth(),b=b.getFullYear();return a+" "+"January February March April May June July August September October November December".split(" ")[c]+", "+b},N=n.Record({type:"unspecified",timeStamp:0,keyCode:0,targetValue:""}),r=function(b){e.call(this,b);this.displayName="TrackingTextarea"};$jscomp.inherits(r,e);r.prototype.focus=function(){this.refs.b.focus()};r.prototype.shouldComponentUpdate=function(b,a){return t(this,b,a)};r.prototype.render=function(){var b=this,c=function(b){b.stopPropagation();
b.preventDefault()},f=function(a){a=new N({type:a.type,timeStamp:a.timeStamp,keyCode:a.keyCode,targetValue:a.target.value});b.props.onNewKeyEvent(a)};return a(this.props.useInputInstead?"input":"textarea",Object.assign({onCopy:c,onCut:c,onPaste:c,onKeyDown:f,onKeyUp:f,autoComplete:"off",className:"form-control",style:this.props.resizable?{}:{resize:"none"}},this.props.otherAttributes,{ref:"b"}))};r.propTypes={onNewKeyEvent:d.func.isRequired,otherAttributes:d.object,resizable:d.bool,useInputInstead:d.bool};
var u=function(b){h.Component.call(this,b);this.displayName="VerbatimTyping";this.state={value:"",highlightRange:0,validationState:"",helpText:this.formatDefaultHelpText(b.typeText)}};$jscomp.inherits(u,h.Component);u.prototype.shouldComponentUpdate=function(b,a){return t(this,b,a)};u.prototype.formatDefaultHelpText=function(b){b=b.split(/\s/).length;return 1===b?"1 word to go\u2026":b+" words to go\u2026"};u.prototype.shouldAllowSubmit=function(){return this.props.typeText&&"has-success"!==this.state.validationState?
(this.refs.b.focus(),this.setState({helpText:"You need to finish typing this before submitting."}),!1):!0};u.prototype.render=function(){var b=this;return a("div",{className:"form-group "+this.state.validationState},a("label",{},this.props.labelText),this.props.typeText?a("blockquote",{},a("p",{},a("span",{className:"unselectable-text","data-content":this.props.typeText.slice(0,this.state.highlightRange),style:{color:"#26ba33"}}),a("span",{className:"unselectable-text","data-content":this.props.typeText.slice(this.state.highlightRange)}))):
null,a(r,{otherAttributes:{value:this.state.value,rows:this.props.rows?this.props.rows:3,onBlur:function(){if(b.props.typeText&&"has-success"!==b.state.validationState){var a=""!==b.state.value;b.setState({value:"",highlightRange:0,validationState:""});a&&b.setState({helpText:"You need to finish typing at one go\u2026"});if(b.props.onRetry)b.props.onRetry()}},onChange:function(a){var c=a.target.value;b.setState({value:c});a=b.props.typeText;var l=L(a.toLowerCase(),c.toLowerCase());b.setState({highlightRange:l});
c=Math.min(a.length,c.length);l===a.length?b.setState({validationState:"has-success",helpText:"Done!"}):l===c?b.setState({validationState:"",helpText:b.formatDefaultHelpText(b.props.typeText.slice(c))}):l<c&&b.setState({validationState:"has-error",helpText:"There is a mistake there\u2026 Please follow the given text closely."})},onFocus:function(){b.props.typeText&&"has-success"!==b.state.validationState&&b.setState({helpText:b.formatDefaultHelpText(b.props.typeText)})},readOnly:"has-success"===this.state.validationState},
ref:"b",onNewKeyEvent:this.props.onNewKeyEvent}),this.state.helpText?a("p",{className:"help-block"},this.state.helpText):null)};u.propTypes={rows:d.number,labelText:d.string.isRequired,typeText:d.string.isRequired,onNewKeyEvent:d.func.isRequired,onRetry:d.func};var q=function(b){e.call(this,b);this.displayName="GenericPage"};$jscomp.inherits(q,e);q.prototype.shouldComponentUpdate=function(b,a){return t(this,b,a)};q.prototype.render=function(){return a("div",{},this.props.preTitleContent||a("div",
{}),a("div",{className:"row"},a("div",{className:"col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2"},a("h1",{className:"text-center"},this.props.title),a("div",{className:"text-center"},this.props.explanation))),this.props.content)};q.propTypes={preTitleContent:d.node,title:d.node.isRequired,explanation:d.node.isRequired,content:d.node.isRequired};var x=function(b){e.call(this,b);this.displayName="Page0Welcome"};$jscomp.inherits(x,e);x.prototype.shouldComponentUpdate=function(b,a){return t(this,
b,a)};x.prototype.render=function(){return void 0===g.ontouchmove?a(q,{title:"Device Unsupported",explanation:a("div",{},a("p",{},"Welcome to River Valley High School Science Lab Undertaking Declaration Site. You will need to perform a signature, therefore to use this website, you need a touchscreen. Your device and/or your browser does not support touch."),a("p",{},"If you are a teacher, please ",a("a",{href:"/admin"},"log in and visit the administrator console"),"."))}):a(q,{preTitleContent:a("div",
{className:"row",style:{padding:"30px 0"}},a("img",{className:"center-block img-responsive",width:200,src:"/static/res/rvcrayon.jpg"})),title:"RVHS Science Lab Undertaking",explanation:a("div",{},a("p",{},"Welcome!"),a("p",{},"In this exericse, you will verify and provide essential personal information. At the same time, you will undertake to agree to the Science Laboratory Rules and endorse your declaration."),a("p",{},"If you have a Personal Learning Device (PLD) with a large screen (tablets instead of phones), it is ",
a("strong",{},"highly recommended")," that you use such a device to proceed due to the convenience afforded by the larger screen.")),content:a("div",{},a("div",{className:"row",style:{padding:"30px 0"}},a("p",{className:"text-center"},a("button",{className:"btn btn-primary",onClick:this.props.onContinueClick},"Continue"))))})};x.propTypes={onContinueClick:d.func.isRequired};var w=function(b){e.call(this,b);this.displayName="ListPickerPage"};$jscomp.inherits(w,e);w.prototype.shouldComponentUpdate=
function(b,a){return t(this,b,a)};w.prototype.render=function(){var b=this;return a(q,{title:this.props.title,explanation:this.props.explanation,content:a("ul",{className:"list-group"},this.props.data.map(function(c,f){return a("li",{key:f,className:"list-group-item",style:{position:"relative"},onClick:function(){b.props.onClick(c)}},b.props.dataFormatter(c),a("span",{className:"glyphicon glyphicon-chevron-right",style:{position:"absolute",display:"block",right:10,top:"50%",transform:"translateY(-50%)",
MsTransform:"translateY(-50%)",WebkitTransform:"translateY(-50%)"}}))}))})};w.propTypes={title:d.node.isRequired,explanation:d.node.isRequired,data:d.object.isRequired,onClick:d.func.isRequired,dataFormatter:d.func.isRequired};var y=function(b){e.call(this,b);this.displayName="Page1ClassChooser"};$jscomp.inherits(y,e);y.prototype.shouldComponentUpdate=function(b,a){return t(this,b,a)};y.prototype.render=function(){return a(w,{title:"Choose Your Class",explanation:a("div",{},a("p",{},"Choose your class from the list below."),
a("p",{},"If you cannot find your class, please seek assistance from your science teacher.")),onClick:this.props.onClassClick,data:this.props.classes,dataFormatter:function(b){return b}})};y.propTypes={onClassClick:d.func.isRequired,classes:d.object.isRequired};var z=function(b){e.call(this,b);this.displayName="Page2StudentChooser"};$jscomp.inherits(z,e);z.prototype.shouldComponentUpdate=function(b,a){return t(this,b,a)};z.prototype.render=function(){return a(w,{title:"Choose Your Name",explanation:a("div",
{},a("p",{},"Choose your name from this list below to access your personal information."),a("p",{},"If you cannot find your name, please seek assistance from your science teacher.")),onClick:this.props.onStudentClick,data:this.props.students,dataFormatter:function(b){return b.first()+". "+b.last()}})};z.propTypes={onStudentClick:d.func.isRequired,students:d.object.isRequired};var A=function(b){e.call(this,b);this.displayName="Page3AskForNric";this.state={partialNric:""}};$jscomp.inherits(A,e);A.prototype.shouldComponentUpdate=
function(b,a){return t(this,b,a)};A.prototype.render=function(){var b=this;return a(q,{title:"Authentication",explanation:a("p",{},"In order to verify your identity, enter the ",a("strong",{},"LAST FOUR")," characters of your NRIC / FIN, e.g. for S9876",a("u",{},"543A"),", enter ",a("kbd",{},"543A"),"."),content:a("div",{},a("div",{className:"form-horizontal"},a("div",{className:"form-group"},a("label",{className:"col-sm-2 control-label",htmlFor:"class"},"Class"),a("div",{className:"col-sm-10"},a("input",
{type:"text",className:"form-control",id:"class",readOnly:!0,value:this.props.currentClass}))),a("div",{className:"form-group"},a("label",{className:"col-sm-2 control-label",htmlFor:"name"},"Name"),a("div",{className:"col-sm-10"},a("input",{type:"text",className:"form-control",id:"name",readOnly:!0,value:this.props.currentStudent.last()}))),a("div",{className:"form-group"},a("label",{className:"col-sm-2 control-label",htmlFor:"nric"},"Partial NRIC"),a("div",{className:"col-sm-10"},a(r,{onNewKeyEvent:this.props.onNewKeyEvent,
otherAttributes:{rows:1,inputMode:"verbatim",placeholder:"e.g. 543A",autoCorrect:"off",value:this.state.partialNric,onChange:function(a){a=a.target.value.trim().toUpperCase().match(/^(?:[0-9](?:[0-9](?:[0-9][JZIHGFEDCBAXWUTRQPNMLK]?)?)?)?/)[0];b.setState({partialNric:a})}},resizable:!1}))),a("div",{className:"form-group"},a("div",{className:"col-sm-offset-2 col-sm-10"},a("button",{className:"btn btn-primary",onClick:function(){if(4!==b.state.partialNric.length)alert("Please enter your partial NRIC before continuing.");
else b.props.onContinueClick(b.state.partialNric)}},"Continue")))))})};A.propTypes={onContinueClick:d.func.isRequired,currentClass:d.string.isRequired,currentStudent:d.object.isRequired};var C=function(b){e.call(this,b);this.displayName="Page4AskForContact";this.state={email:"",phone:"+65 ",emailValidationState:"",phoneValidationState:"",cca1:"",cca2:"",cca3:""}};$jscomp.inherits(C,e);C.prototype.render=function(){var b=this,c=function(a){return function(c){var d={};d["cca"+a]=c.target.value;b.setState(d)}};
return a(q,{title:"Personal Information",explanation:a("p",{},"Check your details shown below are correct and provide your  ",a("strong",{},"frequently used")," email address, mobile number and CCA(s), if any."),content:a("div",{},a("div",{className:"form-horizontal"},a("div",{className:"form-group"},a("label",{className:"col-sm-4 control-label",htmlFor:"chinesename"},"Chinese Name"),a("div",{className:"col-sm-8"},a("input",{type:"text",className:"form-control",id:"chinesename",readOnly:!0,value:this.props.currentStudentChineseName}))),
a("div",{className:"form-group"},a("label",{className:"col-sm-4 control-label",htmlFor:"subj"},"Subject Combinations"),a("div",{className:"col-sm-8"},a("input",{type:"text",className:"form-control",id:"subj",readOnly:!0,value:this.props.currentStudentSubjects}))),a("div",{className:"form-group "+this.state.emailValidationState},a("label",{className:"col-sm-4 control-label",htmlFor:"email"},"Frequently Used Email Address"),a("div",{className:"col-sm-8"},a(r,{onNewKeyEvent:this.props.onNewKeyEventEmail,
useInputInstead:!0,otherAttributes:{id:"email",type:"email",placeholder:"e.g. myname@example.com",autoCorrect:"off",value:this.state.email,onChange:function(a){a=a.target.value.trim();b.setState({email:a});a.length?a.match(/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/)?b.setState({emailValidationState:"has-success"}):b.setState({emailValidationState:"has-error"}):b.setState({emailValidationState:""})}},resizable:!1}))),
a("div",{className:"form-group "+this.state.phoneValidationState},a("label",{className:"col-sm-4 control-label",htmlFor:"phone"},"Mobile Number"),a("div",{className:"col-sm-8"},a(r,{onNewKeyEvent:this.props.onNewKeyEventPhone,useInputInstead:!0,otherAttributes:{id:"phone",type:"tel",autoCorrect:"off",value:this.state.phone,onChange:function(a){var c=a.target.value;a=function(){b.setState({phone:"+65 ",phoneValidationState:""})};4>=c.length?a():4<c.length&&8>=c.length?(c=c.match(/^\+65 (?:[0-9]*)/))?
b.setState({phone:c[0],phoneValidationState:""}):a():8<c.length&&13>=c.length?(c=c.match(/^\+65 ([0-9]{4}) *([0-9]*)/))?b.setState({phone:"+65 "+c[1]+" "+c[2],phoneValidationState:13===c[0].length?"has-success":""}):a():(c=c.match(/^\+65 [0-9]{4} [0-9]{4}/))?b.setState({phone:c[0],phoneValidationState:"has-success"}):a()}},resizable:!1}))),n.List.of(1,2,3).map(function(d){return a("div",{className:"form-group",key:d},a("label",{className:"col-sm-4 control-label",htmlFor:"cca"+d},"CCA / Academy / Others "+
d),a("div",{className:"col-sm-8"},a("select",{className:"form-control",id:"cca"+d,onChange:c(d)},b.props.ccas.groupBy(function(a){return a.get("category")}).map(function(b,c){return b.map(function(b){return a("option",{value:b.get("id"),key:b.get("id")},b.get("name"))}).unshift(a("option",{disabled:!0,key:c},c))}).toList().flatten(!0).unshift(a("option",{value:"",key:"none"},"None")).toJS())))}),a("div",{className:"form-group"},a("div",{className:"col-sm-offset-4 col-sm-8"},a("button",{className:"btn btn-primary",
onClick:function(a){if("has-success"!==b.state.emailValidationState)alert("The email address is incorrect.");else if("has-success"!==b.state.phoneValidationState)alert("The mobile number is incorrect.");else b.props.onContinueClick(b.state.email,b.state.phone,b.state.cca1,b.state.cca2,b.state.cca3)}},"Continue")))))})};C.propTypes={onContinueClick:d.func.isRequired,currentStudentChineseName:d.string.isRequired,currentStudentSubjects:d.string.isRequired,onNewKeyEventEmail:d.func.isRequired,onNewKeyEventPhone:d.func.isRequired,
ccas:d.object.isRequired};var v=function(b){e.call(this,b);this.displayName="Page5AskForSignature";this.state={sketchManager:null,bypassTyping:0}};$jscomp.inherits(v,e);v.prototype.componentDidMount=function(){var b=J(this.refs.canvas);b.setupEventListeners();this.setState({sketchManager:b})};v.prototype.componentWillUnmount=function(){this.state.sketchManager.destroyEventListeners()};v.prototype.shouldComponentUpdate=function(b){return!E(this.props,b)};v.prototype.render=function(){var b=this;return a(q,
{title:a("span",{onClick:function(){b.setState(function(b){return{bypassTyping:b.bypassTyping+1}})}},"Undertaking Declaration"),explanation:a("div",{},a("p",{},"By signing below, I agree that:"),a("p",{},"(a) I have attended the Laboratory Briefing by my science subject teacher(s) and have read and understood the Science Laboratory Rules in the RVHS Student's Handbook"),a("p",{},"(b) I hereby undertake and agree to abide by these rules at all times and I will conduct yourself in a responsible manner when using the laboratory.")),
content:a("div",{},a(u,{ref:"v3",rows:1,labelText:"Type your full name",typeText:this.props.studentName,onNewKeyEvent:this.props.onNewKeyEventGen("name"),onRetry:this.props.onRetryGen("name")}),a(u,{ref:"v4",rows:1,labelText:"Type today\u2019s date",typeText:M(),onNewKeyEvent:this.props.onNewKeyEventGen("date"),onRetry:this.props.onRetryGen("date")}),a("div",{className:"form-group"},a("label",{},"Use your finger to sign below")),a("div",{className:"row"},a("div",{className:"col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2"},
a("canvas",{width:500*F,height:310*F,ref:"canvas",style:{backgroundColor:"#e6f1fe",width:"90%",maxWidth:310,display:"block",margin:"0 auto"}}))),a("div",{className:"row",style:{padding:"30px 0"}},a("div",{className:"text-center"},a("button",{className:"btn btn-primary",onClick:function(){b.state.sketchManager&&b.state.sketchManager.clear()},style:{marginRight:"4em"}},"Clear Signature"),a("button",{className:"btn btn-primary",onClick:function(){if(7===b.state.bypassTyping||n.Range(3,5).every(function(a){return b.refs["v"+
a].shouldAllowSubmit()}))b.state.sketchManager.hasDrawn()?(b.props.onSubmitClick(b.refs.canvas.toDataURL("image/png")),b.state.sketchManager.destroyEventListeners()):alert("You have not signed yet.")}},"Submit"))))})};v.propTypes={onNewKeyEventGen:d.func.isRequired,onRetryGen:d.func.isRequired,onSubmitClick:d.func.isRequired,studentName:d.string.isRequired};var D=function(b){e.call(this,b);this.displayName="Page6Finish"};$jscomp.inherits(D,e);D.prototype.shouldComponentUpdate=function(){return!1};
D.prototype.render=function(){return a(q,{title:"Thank You",explanation:a("div",{},a("p",{},"You have completed the exercise. A copy of the Undertaking Declaration Certificate (UDC) will be sent to your email. You can close this tab now. Have an enriching academic year ahead!")),content:a("div",{})})};d=function(b){e.call(this,b);this.displayName="Page";this.state={currentPage:0,recordedEvents:n.Map(),allClasses:null,allStudentsInClass:null,ccas:null,currentClass:null,currentStudent:null,currentStudentId:null,
currentStudentNric:null,currentStudentChineseName:null,currentStudentSubjects:null,currentStudentEmail:"",currentStudentPhone:"",currentStudentCca1:"",currentStudentCca2:"",currentStudentCca3:""}};$jscomp.inherits(d,e);d.prototype.shouldComponentUpdate=function(b,a){return this.state.currentPage!==a.currentPage};d.prototype.render=function(){var b=this,d=function(a){return function(c){b.setState(function(b){return{recordedEvents:b.recordedEvents.updateIn([a],n.List(),function(b){return b.push(c)})}})}},
e=function(a){return function(c){b.setState(function(b){return{recordedEvents:b.recordedEvents.set(a,n.List())}})}};return a("div",{className:"container"},function(){if(0===b.state.currentPage)return a(x,{onContinueClick:function(){p("Page 0: Continue clicked");k.getJSON("/api/classes").done(function(a){a=n.fromJS(a.data).map(function(a){return a.first()+a.last()});b.setState({allClasses:a,currentPage:1})})}});if(1===b.state.currentPage)return a(y,{onClassClick:function(a){p("Page 1: class clicked",
a);k.getJSON("/api/classes/"+a).done(function(c){c=n.fromJS(c.data);b.setState({allStudentsInClass:c,currentClass:a,currentPage:2})})},classes:b.state.allClasses});if(2===b.state.currentPage)return a(z,{onStudentClick:function(a){p("Page 2: student clicked",a);b.setState({currentStudent:a,currentPage:3})},students:b.state.allStudentsInClass});if(3===b.state.currentPage)return a(A,{onContinueClick:function(a){p("Page 3: Continue clicked",a);k.getJSON("/api/classes/"+b.state.currentClass+"/"+b.state.currentStudent.first(),
{nric:a}).done(function(c){var d=c.data;b.setState({currentStudentNric:a,currentStudentId:d.id});"SubmissionOpen"===d.submission.tag?(b.setState({currentStudentChineseName:d.chinese_name}),k.getJSON("/api/subjects").done(function(a){var c=n.fromJS(a.data);a=n.fromJS(d.subject_combi).map(function(a){return c.find(function(b){return b.get("id")===a}).get("name")}).join(", ")||"-";p(a);b.setState({currentStudentSubjects:a});k.getJSON("/api/ccas").done(function(a){a=n.fromJS(a.data);p(a.groupBy(function(a){return a.get("category")}).map(function(a,
b){return a.map(function(a){return a.get("name")}).unshift(b)}).toList().flatten(!0).unshift("None").toJS());b.setState({ccas:a,currentPage:4})})})):"SubmissionNotOpen"===d.submission.tag?alert("Submission is not open yet. Check back later."):alert("You have already submitted before. You cannot submit again.")}).fail(function(){alert("The NRIC is incorrect.")})},currentClass:b.state.currentClass,currentStudent:b.state.currentStudent,onNewKeyEvent:d("nric")});if(4===b.state.currentPage)return a(C,
{onContinueClick:function(a,c,d,e,f){p("Page 4: Continue clicked",a,c,d,e,f);b.setState({currentStudentEmail:a,currentStudentPhone:c,currentStudentCca1:d,currentStudentCca2:e,currentStudentCca3:f,currentPage:5})},currentStudentSubjects:b.state.currentStudentSubjects,currentStudentChineseName:b.state.currentStudentChineseName,onNewKeyEventEmail:d("email"),onNewKeyEventPhone:d("phone"),ccas:b.state.ccas});if(5===b.state.currentPage)return a(v,{onSubmitClick:function(a){p("Page 5: submit click");a=n.fromJS({nric:b.state.currentStudentNric,
email:b.state.currentStudentEmail,phone:b.state.currentStudentPhone,cca1:b.state.currentStudentCca1,cca2:b.state.currentStudentCca2,cca3:b.state.currentStudentCca3,sig:a,ua:c.navigator.userAgent}).map(function(a,b){return b+"="+c.encodeURIComponent(a).replace("%20","+")}).toList().join("&");p(a);var d=JSON.stringify({recordedEvents:b.state.recordedEvents,userAgent:c.navigator.userAgent,mediaWidth:Math.max(g.documentElement.clientWidth,c.innerWidth||0),mediaHeight:Math.max(g.documentElement.clientHeight,
c.innerHeight||0),screenWidth:c.screen.width,screenHeight:c.screen.height,devicePixelRatio:c.devicePixelRatio||1,pageLoadTime:K,pageSubmitTime:Date.now(),performanceNow:c.performance&&c.performance.now?c.performance.now():null});p(d);d=H.fromByteArray(I.deflate(d));p(d);k.post("/api/students/"+b.state.currentStudentId+"/submit",a,function(){b.setState({currentPage:6})})},studentName:b.state.currentStudent.last(),onNewKeyEventGen:d,onRetryGen:e});if(6===b.state.currentPage)return a(D,{});throw"Internal error.";
}())};m.render(a(d,{}),g.getElementById("body"))})})(window,document,React,ReactDOM,$,Immutable,base64js,pako);
