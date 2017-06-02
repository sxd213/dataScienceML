define("js/lib/keyMirror",["require","exports","module","underscore"],function(require,exports,module){var _=require("underscore"),e=function keyMirror(e){return _(e).chain().map(function(r,e){return[e,e]}).object().value()};module.exports=e});
define("bundles/quick-questions/constants/QuickQuestionsAdminConstants",["require","js/lib/keyMirror"],function(require){var n=require("js/lib/keyMirror");return{ActionTypes:n({REPLACE_ALL_QUESTIONS:null,ANSWER_QUESTION:null,RECEIVE_ALL_EXISTING_RAW_QUESTIONS:null}),PayloadSources:n({SERVER_ACTION:null,VIEW_ACTION:null})}});
define("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher",["require","exports","module","underscore","bundles/quick-questions/constants/QuickQuestionsAdminConstants","js/vendor/Dispatcher"],function(require,exports,module){var _=require("underscore"),n=require("bundles/quick-questions/constants/QuickQuestionsAdminConstants"),e=require("js/vendor/Dispatcher"),s=n.PayloadSources,i=_.extend(new e,{handleViewAction:function handleViewAction(n){var e={source:s.VIEW_ACTION,action:n};this.dispatch(e)},handleServerAction:function handleServerAction(n){var e={source:s.SERVER_ACTION,action:n};this.dispatch(e)}});module.exports=i});
define("bundles/quick-questions/actions/QuestionServerActionCreators",["require","bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher","bundles/quick-questions/constants/QuickQuestionsAdminConstants"],function(require){var s=require("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher"),n=require("bundles/quick-questions/constants/QuickQuestionsAdminConstants"),i=n.ActionTypes;return{receiveAllQuestions:function receiveAllQuestions(n){s.handleServerAction({type:i.RECEIVE_ALL_EXISTING_RAW_QUESTIONS,questions:n})}}});
define("bundles/quick-questions/utils/jsonPath",["require","exports","module","underscore"],function(require,exports,module){var _=require("underscore"),n="arrayIndex",e="objectKey",t=function arrayIndex(e){return{type:n,index:e}},r=function objectKey(n){return{type:e,key:n}},u=function jsonPathToString(t){return"$"+_(t).map(function(t){if(_.isObject(t)&&_.isString(t.type)){if(t.type===n)return"["+t.index+"]";if(t.type===e)return"."+t.key;throw"insane jsonpath type:"+t.type}throw"non object json path element:"+t}).join("")},i=function flattenToArray(n,e){switch(!0){case _.isObject(n):return _.chain(n).map(function(t,u){var n=e.slice();return n.push(r(u)),flattenToArray(t,n)}).flatten(!0).value();case _.isArray(n):return _.chain(n).map(function(r,u){var n=e.slice();return n.push(t(u)),flattenToArray(r,n)}).flatten(!0).value();default:return[[e,JSON.stringify(n)]]}},o=function flatten(n){return _.chain(i(n,[])).map(function(n){return[u(n[0]),n[1]]}).sortBy(0).map(function(n){return n[0]+"="+n[1]}).value().join("&")};module.exports={flatten:o}});
define("nls/page",{es:!0,fr:!0,pt:!0,ru:!0,tr:!0,zh:!0,"zh-hk":"zh-tw","zh-mo":"zh-tw","zh-tw":!0});
define("bundles/quick-questions/utils/APIUtils",["require","bundles/quick-questions/actions/QuestionServerActionCreators","bundles/quick-questions/utils/jsonPath","js/lib/api"],function(require){var e=require("bundles/quick-questions/actions/QuestionServerActionCreators"),t=require("bundles/quick-questions/utils/jsonPath"),i=require("js/lib/api"),s=i("",{type:"rest"});return{getAllQuestions:function getAllQuestions(t){t?s.post("/api/quickQuestions.v1?action=getAll&moduleId="+t,{success:e.receiveAllQuestions}):s.post("/api/quickQuestions.v1?action=getAllGlobal",{success:e.receiveAllQuestions})},replaceAllQuestions:function replaceAllQuestions(e){s.post("/api/quickQuestions.v1?action=setQuestions",{dataType:"json",data:e})},postResponse:function postResponse(e,i){var n=t.flatten(e);s.put("/api/quickQuestionResponses.v1/"+n,{dataType:"json",data:i})}}});
define("bundles/quick-questions/constants/Config",["require","exports","module"],function(require,exports,module){module.exports={EPIC_NAMESPACE:"QuickQuestions",parameters:{OPT_IN_STYLE:"OptInStyle"},optInStyles:{NO_MODAL:"NoModal",NONE:"None",PROMINENT_OPT_IN:"ProminentOptIn",PROMINENT_OPT_OUT:"ProminentOptOut"}}});
define("bundles/quick-questions/utils/ABTestEvents",["require","exports","module","underscore","js/app/multitrackerSingleton","bundles/epic/client","bundles/quick-questions/constants/Config"],function(require,exports,module){var _=require("underscore"),s=require("js/app/multitrackerSingleton"),t=require("bundles/epic/client"),e=require("bundles/quick-questions/constants/Config"),o=function _getConfiguredStyle(){var n=t.get(e.EPIC_NAMESPACE,e.parameters.OPT_IN_STYLE);return _.chain(e.optInStyles).values().contains(n).value()||(n=e.optInStyles.NO_MODAL),n},n=function _pushEvent(e,n,o){s.push(["up.quick_question."+e+"."+n,o])};module.exports=function(e,s){return{showQuestion:function showQuestion(o,t){n("render","show_question",{courseId:s,moduleId:e,questionId:o,questionOrder:t})},answerQuestion:function answerQuestion(o,t){n("click","answer_question",{courseId:s,moduleId:e,questionId:o,answerId:t})},showModal:function showModal(){var t=o();n("render","show_modal",{courseId:s,moduleId:e,interfaceBucket:t})},answerModal:function answerModal(t){var u=o();n("click","answer_modal",{courseId:s,moduleId:e,interfaceBucket:u,buttonName:t})},buttonNames:{CLOSE_MODAL:"closeModal",CONTINUE_TO_COURSE:"continueToCourse",OPT_IN:"optIn",CLOSE_AT_THANK_YOU:"closeThankYou"}}}});
define("bundles/quick-questions/constants/ModalFlowConstants",["require","js/lib/keyMirror"],function(require){var l=require("js/lib/keyMirror");return{ActionTypes:l({SHOW_END_OF_MODULE:null,OPT_INTO_QUICK_QUESTIONS:null,COMPLETE_QUICK_QUESTIONS:null,X_OUT_MODAL:null}),ModalStates:l({CLOSED:null,OPT_IN_PAGE:null,QUICK_QUESTION_FLOW:null,THANK_YOU_PAGE:null})}});
define("pages/open-course/statefulModal/actions/ModalFlowActionCreators",["require","exports","module","bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher","bundles/quick-questions/utils/APIUtils","bundles/quick-questions/utils/ABTestEvents","bundles/quick-questions/constants/ModalFlowConstants"],function(require,exports,module){var n=require("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher"),s=require("bundles/quick-questions/utils/APIUtils"),o=require("bundles/quick-questions/utils/ABTestEvents"),i=require("bundles/quick-questions/constants/ModalFlowConstants"),t=i.ActionTypes,e;module.exports={openModal:function openModal(module,i){e=o(module.id,module.courseId),e.showModal(),n.handleViewAction({type:t.SHOW_END_OF_MODULE,moduleIndex:i,module:module}),s.getAllQuestions(module.id)},optIn:function optIn(){e&&e.answerModal(e.buttonNames.OPT_IN),n.handleViewAction({type:t.OPT_INTO_QUICK_QUESTIONS})},completeQuickQuestions:function completeQuickQuestions(){n.handleViewAction({type:t.COMPLETE_QUICK_QUESTIONS})},continueOptOut:function continueOptOut(){e&&e.answerModal(e.buttonNames.CONTINUE_TO_COURSE),n.handleViewAction({type:t.X_OUT_MODAL})},closeThankYou:function closeThankYou(){e&&e.answerModal(e.buttonNames.CLOSE_AT_THANK_YOU),n.handleViewAction({type:t.X_OUT_MODAL})},closeModal:function closeModal(){e&&e.answerModal(e.buttonNames.CLOSE_MODAL),n.handleViewAction({type:t.X_OUT_MODAL})}}});
define("pages/open-course/statefulModal/components/CongratulationsPage",["require","exports","module","classnames","react-with-addons","bundles/quick-questions/constants/Config","pages/open-course/statefulModal/actions/ModalFlowActionCreators"],function(require,exports,module){var s=require("classnames"),e=require("react-with-addons"),t=require("bundles/quick-questions/constants/Config"),o=require("pages/open-course/statefulModal/actions/ModalFlowActionCreators"),n=e.createClass({displayName:"CongratulationsPage",propTypes:{moduleIndex:e.PropTypes.number.isRequired,module:e.PropTypes.shape({completedLessonCount:e.PropTypes.number.isRequired,passedQuizCount:e.PropTypes.number.isRequired,name:e.PropTypes.string.isRequired}).isRequired,qqOptInStyle:e.PropTypes.string.isRequired},render:function render(){var n=this.props.qqOptInStyle,l=s("qq-opt-in",{hidden:n===t.optInStyles.NONE,"prominent-opt-in":n===t.optInStyles.PROMINENT_OPT_IN,"prominent-opt-out":n===t.optInStyles.PROMINENT_OPT_OUT}),a=s("continue-btn",{muted:n===t.optInStyles.PROMINENT_OPT_IN});return e.createElement("div",null,e.createElement("div",{className:"completed-module"},e.createElement("div",{className:"qq-bold"},"You have completed Module ",this.props.moduleIndex+1),e.createElement("div",null,this.props.module.name)),e.createElement("div",{className:"module-completion"},e.createElement("div",null,this.props.module.completedLessonCount," ",1===this.props.module.completedLessonCount?"lesson":"lessons"," completed"),e.createElement("div",null,this.props.module.passedQuizCount," ",1===this.props.module.passedQuizCount?"quiz":"quizzes"," passed")),e.createElement("button",{onClick:o.continueOptOut,className:a},"Continue"),e.createElement("div",{className:l},e.createElement("div",{className:"qq-opt-in-centered"},e.createElement("div",{className:"qq-call-out"},"Help us improve our courses by answering a few questions!"),e.createElement("div",null,e.createElement("span",{className:"qq-opt-in-text"},"It'll only take two minutes and you won't be taken away from this window"),e.createElement("button",{onClick:o.optIn,className:"qq-opt-in-btn"},"Yes, I'd like to"),e.createElement("div",{className:"clear-fix"})))))}});module.exports=n});
define("bundles/quick-questions/stores/TriggeredQuestionsStore",["require","exports","module","underscore","bundles/quick-questions/utils/ABTestEvents","js/vendor/EventEmitter","bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher","bundles/quick-questions/constants/QuickQuestionsAdminConstants","bundles/quick-questions/constants/ModalFlowConstants","bundles/phoenix/template/models/userIdentity"],function(require,exports,module){var _=require("underscore"),v=require("bundles/quick-questions/utils/ABTestEvents"),l=require("js/vendor/EventEmitter"),m=require("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher"),d=require("bundles/quick-questions/constants/QuickQuestionsAdminConstants"),h=require("bundles/quick-questions/constants/ModalFlowConstants"),g=require("bundles/phoenix/template/models/userIdentity"),r=d.ActionTypes,a=h.ActionTypes,i="change",u,o,n=[],s=[],e=-1,c=!1,q=function _setModuleId(e){u=e},f=function _setCourseId(e){o=e},p=function _setQuestions(t){n=_(t).sortBy("subject").reverse(),n.length>1&&(c=!0),s=_(n).map(function(){return!1}),e=0},C=function _currentlyInBounds(){return e<n.length},k=function _incrementQuestion(){if(e+1>=n.length)return void(c=!1);e++},Q=function _markAnswered(){C()&&(s[e]=!0)},t=_.extend({},l.prototype,{emitChange:function emitChange(){this.emit(i)},addChangeListener:function addChangeListener(e){this.on(i,e)},removeChangeListener:function removeChangeListener(e){this.removeListener(i,e)},getCurrentQuestionToShow:function getCurrentQuestionToShow(){var t=n[e],s=v(u,o);return s.showQuestion(t.id,e),t},getCurrentQuestionIndex:function getCurrentQuestionIndex(){return e},getCurrentQuestionCount:function getCurrentQuestionCount(){return n.length},getQuestionAnsweredStates:function getQuestionAnsweredStates(){return s.slice()},getCurrentResponseContext:function getCurrentResponseContext(){var e={moduleId:u,userId:g.get("id"),courseId:o};return e},getCurrentResponse:function getCurrentResponse(t){var s=new Date,i={questionId:n[e].id,chosenOptionId:t,time:s.toLocaleString()};return i}});t.dispatchToken=m.register(function(n){var e=n.action;switch(e.type){case r.RECEIVE_ALL_EXISTING_RAW_QUESTIONS:p(e.questions),t.emitChange();break;case r.ANSWER_QUESTION:Q(),k(),t.emitChange();break;case a.SHOW_END_OF_MODULE:q(e.module.id),f(e.module.courseId),t.emitChange()}}),module.exports=t});
define("bundles/quick-questions/actions/ResponseActionCreators",["require","bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher","bundles/quick-questions/constants/QuickQuestionsAdminConstants","bundles/quick-questions/utils/APIUtils","bundles/quick-questions/utils/ABTestEvents"],function(require){var s=require("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher"),e=require("bundles/quick-questions/constants/QuickQuestionsAdminConstants"),n=require("bundles/quick-questions/utils/APIUtils"),i=require("bundles/quick-questions/utils/ABTestEvents"),t=e.ActionTypes;return{postResponse:function postResponse(e,u){var o=i(e.moduleId,e.courseId);o.answerQuestion(u.questionId,u.chosenOptionId),s.handleViewAction({type:t.ANSWER_QUESTION,response:u,responseContext:e}),n.postResponse(e,u)}}});
define("bundles/quick-questions/PropValidators",["require","exports","module","react-with-addons"],function(require,exports,module){var e=require("react-with-addons"),s=e.PropTypes.string.isRequired,r=e.PropTypes.shape({id:e.PropTypes.string.isRequired,order:e.PropTypes.number.isRequired,text:e.PropTypes.string.isRequired}).isRequired,i=e.PropTypes.shape({subject:e.PropTypes.string.isRequired,id:s,prompt:e.PropTypes.string.isRequired,options:e.PropTypes.arrayOf(r).isRequired}).isRequired,p=e.PropTypes.shape({questionId:s,chosenOptionId:e.PropTypes.string.isRequired}).isRequired,o=e.PropTypes.shape({moduleId:e.PropTypes.string.isRequired,userId:e.PropTypes.number.isRequired,courseId:e.PropTypes.string.isRequired}),t=e.PropTypes.shape({subjects:e.PropTypes.arrayOf(i).isRequired}).isRequired;module.exports={questionId:s,questionOption:r,question:i,questionResponse:p,responseContext:o,triggeredQuestion:t}});
define("bundles/quick-questions/components/Question",["require","exports","module","classnames","react-with-addons","underscore","bundles/quick-questions/PropValidators","classnames"],function(require,exports,module){var n=require("classnames"),e=require("react-with-addons"),_=require("underscore"),t=require("bundles/quick-questions/PropValidators"),n=require("classnames"),s=e.PropTypes.arrayOf(e.PropTypes.bool).isRequired,i=e.createClass({displayName:"QuestionProgressIndicator",propTypes:{currentIndex:e.PropTypes.number.isRequired,questionCount:e.PropTypes.number.isRequired,questionAnsweredStates:s},render:function render(){var t=this.props.questionAnsweredStates.slice();return t.length>this.props.currentIndex&&(t[this.props.currentIndex]=!0),e.createElement("div",{className:"question-progress"},e.createElement("span",null," ",this.props.currentIndex+1," of ",this.props.questionCount," "),e.createElement("span",null,_(t).map(function(t){return t?e.createElement("i",{className:"icon-circle"}):e.createElement("i",{className:"icon-circle-blank"})})))}}),r=e.createClass({displayName:"ResponseChoice",propTypes:{option:t.questionOption,onSubmit:e.PropTypes.func.isRequired},getInitialState:function getInitialState(){return{selected:!1}},render:function render(){var t=n({"feedback-selected":this.state.selected});return e.createElement("li",{key:this.props.option.id,className:t,onClick:this.onClick},e.createElement("div",{className:"centered"},e.createElement("span",null,this.props.option.text)))},onClick:function onClick(){setTimeout(this.submitSelection.bind(this),500,this.props.option),this.setState({selected:!0})},submitSelection:function submitSelection(){this.setState({selected:!1}),this.props.onSubmit(this)}}),o=e.createClass({displayName:"Question",propTypes:{question:t.question,currentIndex:e.PropTypes.number.isRequired,questionCount:e.PropTypes.number.isRequired,questionAnsweredStates:s,onSubmit:e.PropTypes.func.isRequired},_handleClick:function _handleClick(e){this.props.onSubmit(e.props.option)},render:function render(){var t=_.chain(this.props.question.options).sortBy("order").map(function(t){return e.createElement(r,{option:t,onSubmit:this._handleClick})},this).value();return e.createElement("div",{className:"quick-question-flow"},e.createElement(i,{currentIndex:this.props.currentIndex,questionCount:this.props.questionCount,questionAnsweredStates:this.props.questionAnsweredStates}),e.createElement("div",{className:"clear-fix"}),e.createElement("div",{className:"question-prompt-outer"},e.createElement("div",{className:"question-prompt-index"},this.props.currentIndex+1),e.createElement("div",{className:"question-prompt"},e.createElement("span",{className:"question-prompt-text"},this.props.question.prompt),e.createElement("ul",{className:"question-options"},t))))}});module.exports=o});
define("bundles/quick-questions/components/QuestionFlow",["require","exports","module","react-with-addons","bundles/quick-questions/stores/TriggeredQuestionsStore","bundles/quick-questions/actions/ResponseActionCreators","pages/open-course/statefulModal/actions/ModalFlowActionCreators","bundles/quick-questions/components/Question"],function(require,exports,module){var t=require("react-with-addons"),e=require("bundles/quick-questions/stores/TriggeredQuestionsStore"),s=require("bundles/quick-questions/actions/ResponseActionCreators"),o=require("pages/open-course/statefulModal/actions/ModalFlowActionCreators"),u=require("bundles/quick-questions/components/Question"),n=function getStateFromStores(){return{currentQuestion:e.getCurrentQuestionToShow(),currentQuestionIndex:e.getCurrentQuestionIndex(),questionCount:e.getCurrentQuestionCount(),questionAnsweredStates:e.getQuestionAnsweredStates()}},i=t.createClass({displayName:"QuestionFlow",getInitialState:function getInitialState(){return n()},componentDidMount:function componentDidMount(){e.addChangeListener(this._onChange)},componentWillUnmount:function componentWillUnmount(){e.removeChangeListener(this._onChange)},_handleSubmit:function _handleSubmit(t){var n=e.getCurrentResponse(t.id),u=e.getCurrentResponseContext();this.state.currentQuestionIndex>=this.state.questionCount-1&&o.completeQuickQuestions(),s.postResponse(u,n)},render:function render(){if(!this.state.currentQuestion)return t.createElement("div",null);return t.createElement(u,{onSubmit:this._handleSubmit,question:this.state.currentQuestion,questionCount:this.state.questionCount,currentIndex:this.state.currentQuestionIndex,questionAnsweredStates:this.state.questionAnsweredStates})},_onChange:function _onChange(){this.setState(n())}});module.exports=i});
define("pages/open-course/statefulModal/stores/ModalFlowStore",["require","exports","module","underscore","js/vendor/EventEmitter","bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher","bundles/quick-questions/constants/ModalFlowConstants"],function(require,exports,module){var _=require("underscore"),h=require("js/vendor/EventEmitter"),c=require("bundles/quick-questions/dispatcher/QuickQuestionsAdminDispatcher"),u=require("bundles/quick-questions/constants/ModalFlowConstants"),n=u.ActionTypes,t=u.ModalStates,o="change",i=t.CLOSED,r,a,s=function _setModalState(e){i=e},d=function _setModuleIndex(e){r=e},l=function _setModule(module){a=module},e=_.extend({},h.prototype,{getModalState:function getModalState(){return i},getModule:function getModule(){return a},getModuleIndex:function getModuleIndex(){return r},emitChange:function emitChange(){this.emit(o)},addChangeListener:function addChangeListener(e){this.on(o,e)},removeChangeListener:function removeChangeListener(e){this.removeListener(o,e)}});e.dispatchToken=c.register(function(i){var o=i.action;switch(o.type){case n.SHOW_END_OF_MODULE:s(t.OPT_IN_PAGE),l(o.module),d(o.moduleIndex),e.emitChange();break;case n.OPT_INTO_QUICK_QUESTIONS:s(t.QUICK_QUESTION_FLOW),e.emitChange();break;case n.COMPLETE_QUICK_QUESTIONS:s(t.THANK_YOU_PAGE),e.emitChange();break;case n.X_OUT_MODAL:s(t.CLOSED),e.emitChange()}}),module.exports=e});
define("pages/open-course/statefulModal/components/EndOfModuleModal",["require","exports","module","underscore","react-with-addons","bundles/epic/client","pages/open-course/statefulModal/components/CongratulationsPage","pages/open-course/statefulModal/actions/ModalFlowActionCreators","bundles/quick-questions/components/QuestionFlow","bundles/quick-questions/stores/TriggeredQuestionsStore","bundles/quick-questions/constants/Config","bundles/quick-questions/constants/ModalFlowConstants","pages/open-course/statefulModal/stores/ModalFlowStore"],function(require,exports,module){var _=require("underscore"),e=require("react-with-addons"),i=require("bundles/epic/client"),u=require("pages/open-course/statefulModal/components/CongratulationsPage"),s=require("pages/open-course/statefulModal/actions/ModalFlowActionCreators"),r=require("bundles/quick-questions/components/QuestionFlow"),a=require("bundles/quick-questions/stores/TriggeredQuestionsStore"),t=require("bundles/quick-questions/constants/Config"),c=require("bundles/quick-questions/constants/ModalFlowConstants"),n=require("pages/open-course/statefulModal/stores/ModalFlowStore"),o=c.ModalStates,l=function getStateFromStores(){return{hasQuestions:a.getCurrentQuestionCount()>0,module:n.getModule(),moduleIndex:n.getModuleIndex(),modalState:n.getModalState()}},d=e.createClass({displayName:"EndOfModuleModal",getInitialState:function getInitialState(){return l()},componentDidMount:function componentDidMount(){a.addChangeListener(this._onChange),n.addChangeListener(this._onChange)},componentWillUnmount:function componentWillUnmount(){a.removeChangeListener(this._onChange),n.removeChangeListener(this._onChange)},_onChange:function _onChange(){this.setState(l())},render:function render(){var a=function wrap(t){return e.createElement("div",null,e.createElement("div",{id:"stateful-modal-overlay",onClick:s.closeModal}),e.createElement("div",{id:"stateful-modal-content"},e.createElement("div",{id:"stateful-modal-x-out"},e.createElement("span",{onClick:s.closeModal},"✕")),t))},n=i.get(t.EPIC_NAMESPACE,t.parameters.OPT_IN_STYLE);if(_.chain(t.optInStyles).values().contains(n).value()||(n=t.optInStyles.NO_MODAL),n===t.optInStyles.NO_MODAL)return e.createElement("div",null);switch(this.state.modalState){case o.CLOSED:return e.createElement("div",null);case o.OPT_IN_PAGE:var c=this.state.hasQuestions,l;return l=c?n:t.optInStyles.NONE,a(e.createElement(u,{module:this.state.module,moduleIndex:this.state.moduleIndex,qqOptInStyle:l}));case o.QUICK_QUESTION_FLOW:return a(e.createElement(r,null));case o.THANK_YOU_PAGE:return a(e.createElement("div",{className:"qq-thank-you"},e.createElement("h1",null,"Thank you for your help"),e.createElement("h2",null,"Your input will help us make the course better for learners like yourself."),e.createElement("button",{onClick:s.closeThankYou},"Continue")))}}});module.exports=d});
define("pages/open-course/statefulModal/courseGradeListener",["require","exports","module","underscore","react-with-addons","pages/open-course/statefulModal/actions/ModalFlowActionCreators","pages/open-course/statefulModal/components/EndOfModuleModal"],function(require,exports,module){var _=require("underscore"),e=require("react-with-addons"),t=require("pages/open-course/statefulModal/actions/ModalFlowActionCreators"),o=require("pages/open-course/statefulModal/components/EndOfModuleModal");module.exports=function(u,s,r){e.render(e.createElement(o,null),window.document.getElementById("stateful-modal"));var d=r.getModules(),a={};_(u).each(function(d,t){var module=r.getModuleForItem(t),o=module.getItemMetadatas(),e=o.filter(function(e){return e.isGradable()});if(e){var n=_(e).filter(function(e){var t=s.getItemGrade(e.id);return t.isPassing()});e.length===n.length&&(a[module.id]=module)}});var n=d.chain().filter(function(module){return a[module.id]}).last().value(),l=d.chain().map(function(module,e){return a[module.id]?e:void 0}).filter(function(e){return void 0!==e}).last().value(),i=n.getLessons().length,c=s.get("moduleGrades")[n.get("id")].passableItemsCount,module={name:n.get("name"),id:n.get("id"),courseId:n.get("course").get("id"),completedLessonCount:i,passedQuizCount:c};t.openModal(module,l)}});