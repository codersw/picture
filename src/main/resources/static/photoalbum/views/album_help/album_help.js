$(function () {

    // 下一步
   $(".operate_btn").on("click", function () {
       var _this = $(this);
           helpId = _this.attr("help-id");

       if (helpId == 3) {
           close();
           return
       }
       $("body").css('margin-left', - helpId * 100 + '%')
   });
    function close() {
        // location.href = '../settings/settings.html';
        window.open("../settings/settings.html",'top')
    /*    var userAgent = navigator.userAgent;
        console.log(userAgent);
        if (userAgent.indexOf('Android') > -1 || userAgent.indexOf('Linux') > -1) {
            console.log(1);
            window.opener = null;
            window.open('about:blank', '_self', '');
            window.close();
        } else if (userAgent.indexOf("Firefox") != -1 || userAgent.indexOf("Chrome") != -1) {
            // window.location.href = "about:blank";
        } else {
           /!* window.opener = null;
            window.open("about:blank", "_self");
            window.close();*!/
            console.log(3);
        }*/
    }
});