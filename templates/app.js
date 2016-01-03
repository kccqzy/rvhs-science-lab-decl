"use strict";
((window, document, $) => {
$(function () {
    var submitClassName, submitIndexNumber, submitNric;
    var studentData;

    function CanvasSketch(canvas) {
        var scaleFactor = window.devicePixelRatio || 1;
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


    var PageController = function () {
        var preAnimateHandlers = [], postAnimateHandlers = [], currentPage;

        var pageAnimateFromTo = function (currentPage, newPage) {
            $.each({removeClass: currentPage, addClass: newPage}, function (prop, page) {
                var left = $("#page" + (page - 1));
                left && left[prop]("prev");
                var active = $("#page" + page);
                active && active[prop]("active");
                var right = $("#page" + (page + 1));
                right && right[prop]("next");
            });
        };

        return {
            register: function (page, preAnimate, postAnimate) {
                preAnimateHandlers[page] = preAnimate;
                postAnimateHandlers[page] = postAnimate;
                return this;
            },

            forward: function () {
                ++currentPage && $("#back-button").removeClass("donotpresent").off("tap").on("tap", this.backward);
                preAnimateHandlers[currentPage] && preAnimateHandlers[currentPage]();
                pageAnimateFromTo(currentPage - 1, currentPage);
                postAnimateHandlers[currentPage] && postAnimateHandlers[currentPage]();
            },

            backward: function () {
                --currentPage || $("#back-button").addClass("donotpresent").off("tap");
                preAnimateHandlers[currentPage] && preAnimateHandlers[currentPage]();
                pageAnimateFromTo(currentPage + 1, currentPage);
                postAnimateHandlers[currentPage] && postAnimateHandlers[currentPage]();
            },

            run: function () {
                currentPage = -1;
                this.forward();
            }
        };
    };

    var pageController = PageController();

    pageController
        .register(
            0, function () {
                if (document.ontouchmove === undefined) {
                    $("#page0 .main-title").text("Device Unsupported");
                    $("#page0 .explanation").html('Welcome to River Valley High School Science Lab Undertaking Declaration Site. You will need to perform a signature, therefore to use this website, you need a touchscreen. Your device and/or your browser does not support touch. <br>If you are a teacher, please <a href="/admin">login in and visit the administrator console</a>.');
                    $("#page0 .interactive-content").empty();
                }
            }, function () {
                $("#form").off("submit").on("submit", function () {
                    pageController.forward();
                    return false;
                });
            })

        .register(
            1, function () {
                $("#page1 .interactive-content ul").empty();
            }, function () {
                $.getJSON("/api/classes").done(function (classes) {
                    $.each(classes.data, function (_, className) {
                        $("#page1 .interactive-content ul").append($("<li/>").attr("id", className.join("")).text(className.join("")));
                    });
                    $("#page1 .interactive-content ul").off("tap").on("tap", "li", function () {
                        submitClassName = $(this).attr("id");
                        pageController.forward();
                        return false;
                    });
                });
            })

        .register(
            2, function () {
                if (!submitClassName) throw 'No class name';
                $("#page2 .interactive-content ul").empty();
            }, function () {
                $.getJSON("/api/classes/" + submitClassName).done(function (students) {
                    $.each(students.data, function(_, studentInfo) {
                        $("#page2 .interactive-content ul").append($("<li/>").attr("id", "student" + studentInfo[0]).text(studentInfo[0] + ". " + studentInfo[1]));
                    });
                    $("#page2 .interactive-content ul").off("tap").on("tap", "li", function () {
                        submitIndexNumber = $(this).attr("id").slice(7);
                        pageController.forward();
                        return false;
                    });
                });
            })

        .register(
            3, function () {
                if (!submitClassName || !submitIndexNumber) throw 'No class name or index number';
            }, function () {
                $("#table-class").text(submitClassName);
                $("#table-index").text(submitIndexNumber);
                var canocNric = function (s) {
                    var match = s.trim().toUpperCase().match(/^(?:[STFG][0-9]{0,4})?[0-9]{3}[JZIHGFEDCBAXWUTRQPNMLK]$/);
                    return match && match[0];
                };
                $("#submit-nric").off("change").on("change", function () {
                    var cano = canocNric($(this).val());
                    $(this)[cano ? "removeClass" : "addClass"]("invalid");
                    cano && $(this).val(cano);
                });
                $("#form").off("submit").on("submit", function () {
                    var okay = canocNric($("#submit-nric").val());
                    if (okay) {
                        submitNric = okay;
                        console.log(submitNric);
                        $.getJSON("/api/classes/" + submitClassName + "/" + submitIndexNumber, {nric: submitNric}).done (function (student) {
                            studentData = student.data;
                            if (studentData.submission.tag === "SubmissionOpen") {
                                pageController.forward();
                            } else if (studentData.submission.tag === "SubmissionNotOpen") {
                                alert("Submission is not open yet. Check back later.");
                            } else {
                                alert("You have already submitted before. You cannot submit again.");
                            }
                        }).fail(function () {
                            alert("The NRIC is incorrect.");
                        });
                    } else {
                        alert("The NRIC is incorrect.");
                    }
                    return false; // stopPropagation, preventDefault
                });
            })

        .register(
            4, function () {
                if (!submitClassName || !submitIndexNumber) throw 'No class name or index number';
            }, function () {
                    $("#table-chinesename").text(studentData.chinese_name);
                    $.getJSON("/api/subjects").done(function (subjectInfo) {
                        var combi = studentData.subject_combi;
                        console.log(combi);
                        $("#table-subjects").text(
                            $.map(combi, function(sid) {
                                console.log(_.find(subjectInfo.data, function (su) { return su.id === sid; }).name);
                                return _.find(subjectInfo.data, function (su) { return su.id === sid; }).name;
                            }).join(", ") || "—"
                        );
                    });
                    $.getJSON("/api/ccas").done(function (ccaInfo) {
                        $.each([1,2,3], function (wtf, ccaIndex) {
                            var select = '<select name=cca' + ccaIndex + '>';
                            var options = _.flatten(_.map(_.groupBy(ccaInfo.data, "category"), function (ccas, category) {
                                return [$("<option disabled>" + category + "</option>")].concat(_.map(ccas, function (cca) {
                                    return $('<option value="' + cca.id + '"' + '>' + cca.name + '</option>');
                                }));
                            }));
                            options.unshift($('<option value="">None</option>'));
                            $("#table-cca" + ccaIndex).empty().append($(select).append(options));
                        });
                    });
                    var canonicalisers = {
                        "#submit-email": function (s) {
                            var match = s.trim().match(/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/);
                            // https://html.spec.whatwg.org/multipage/forms.html#e-mail-state-(type=email)
                            return match && match[0];
                        },
                        "#submit-phone": function (s) {
                            var match = s.trim().match(/^(?:\+65)? *([0-9]{4}) *([0-9]{4})$/);
                            return match && "+65 " + match.slice(1).join(" ");
                        }
                    };

                    $.each(canonicalisers, function (sel, func) {
                        $(sel).off("change").on("change", function () {
                            var cano = func($(this).val());
                            $(this)[cano ? "removeClass" : "addClass"]("invalid");
                            cano && $(this).val(cano);
                        });
                    });

                    $("#form").off("submit").on("submit", function () {
                        var okay = true;
                        $.each(canonicalisers, function (sel, func) {
                            return okay = okay && !!func($(sel).val());
                        });
                        console.log(okay);
                        okay ? pageController.forward() : alert("The email address or phone number is incorrect. Please correct this before continuing.");
                        return false; // stopPropagation, preventDefault
                    });
            })

        .register(
            5, function () {
                if (!submitClassName || !submitIndexNumber) throw 'No class name or index number';
                $("#page5 .interactive-content .canvas").empty();
            }, function () {
                // Put a <canvas> element and then initialise a CanvasSketch object to handle the drawing.
                let scaleFactor = window.devicePixelRatio || 1;
                $("#page5 .interactive-content .canvas").on("scrollstart", false).append($("<canvas/>").attr("width", 500 * scaleFactor).attr("height", 310 * scaleFactor));
                let canvas = $("#page5 .interactive-content canvas").get(0);
                let sketchManager = CanvasSketch(canvas);

                $("#page5").on("scrollstop", sketchManager.setupEventListeners).on("webkitTransitionEnd", sketchManager.setupEventListeners);

                $("#form").off("submit").on("submit", function () {
                    if (sketchManager.hasDrawn()) {
                        $("#submit-signature").val(canvas.toDataURL("image/png"));
                        $("#submit-ua").val(navigator.userAgent);
                        $.post("/api/students/" + studentData.id + "/submit", $(this).serialize(), function () {
                            sketchManager.destroyEventListeners();
                            pageController.forward();
                        });
                    } else {
                        alert("You have not signed yet.");
                    }
                    return false;
                });
            })

        .register(
            6, function () {
            $("#back-button").addClass("donotpresent").off("tap");
        })
        .run();
});
})(window, document, $);



// Local variables:
// eval: (add-hook 'after-save-hook (lambda () (shell-command "es6c -w app.js > app.es5.js") (shell-command "es6c app.js > ../static/app.min.js")) nil t)
// End:
