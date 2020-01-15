//http://192.168.4.169:8060/swagger-ui.html
// var serverHost = 'https://photoalbum.517api.cn';
// var serverHost = 'http://192.168.4.169:8060';
// 全局变量
var serverHost = 'http://192.168.4.169:8060',
    id = getLogin(),
    orgId = 0;
// 提示框
function showTips(tips) {
    var $tips = $('.tips');
    $tips.show();
    $('.tips_msg').text(tips);
    setTimeout(function () {
        $tips.hide();
    }, 2000)
}

// 获取本地用户信息
function getOrigId() {
    return JSON.parse(localStorage.getItem('USERS'));
}

// 获取登陆状态
function getLogin() {
    // 获取cookie
    var userIdName = 'useridcookiename=';
    if(document.cookie.indexOf(userIdName) !== -1) {
        $("#container").show();
        $(".unLogin").hide();
        var id = document.cookie.slice(document.cookie.indexOf(userIdName) + 17,  document.cookie.indexOf(userIdName) + 22);
        document.cookie = 'userId' + '=' + id + ';';
        $(".un_login").hide();
        return id
    }else {
        $(".un_login").show();
        // APICard 脚本，获取cookie
        var flag = sessionStorage.getItem('FLAG');
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = 'https://apicard248.517.cn/card/v2.0/api.aspx?cardaction=getcard&cookie=currentsitetoken_cms&userid=useridcookiename&siteid=10&typeid=5';

        if (flag && (new Date().getTime() - sessionStorage.getItem('FLAG') > 300)) {
            sessionStorage.removeItem('FLAG');
            return false
        }
        if (!flag) {
            var date = new Date().getTime();
            sessionStorage.setItem('FLAG', JSON.stringify(date));
            $("body").append(script)
        }
    }
}
