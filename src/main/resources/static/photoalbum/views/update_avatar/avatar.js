$(function () {
    // 监听上传动作
    $("#uploadfile").change(function () {
        $(".loader_bg").show();
        readFile(this);
    });
    // 请求当前头像
    function getAvatar() {
        $.ajax({
            url: serverHost + '/face/' + id,
            type: 'get',
            headers: { 'userId': id },
            data: { userId: id },
            success: function (res) {
                if (res.code === 200) {
                    $('.avatar img').attr('src', res.data ? res.data.imageUrl : '../../common/img/default_avatar.png');
                }
            }
        })
    }
    getAvatar();

    //处理图片并添加都dom中的函数
    var readFile = function(obj) {
        // 获取input里面的文件组
        var fileList = obj.files[0];
        
        if (!fileList.name.match(/.jpg|.gif|.png|.bmp/i)) {
            showTips('只支持图片上传');
            $(".loader_bg").hide();
            return
        }

        var formData = new FormData();
        formData.append('file', fileList);
        formData.append('userId', id);
        $.ajax({
            url: serverHost + '/face',
            type: 'post',
            processData: false,
            contentType: false,
            dataType: 'json',
            data: formData,
            headers: { 'userId': id },
            success: function (res) {
                if (res.code === 200) {
                    showTips(res.message);
                    $('.avatar img').attr('src', res.data.imageUrl);
                } else {
                    showTips(res.message);
                }
                $(".loader_bg").hide();
            }
        });
    };
    $(".close_img").on('click', function () {
        window.open("../settings/settings.html",'top')
    })
});