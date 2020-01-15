var imgUrl = [],
    topTitle = '';
$(function () {
    var pic_nums = 0,
        $container = $('.img_container'),
        resData,
        imgWidth = document.body.clientWidth;
    if (imgWidth > 760) {
        imgWidth = 1520
    } else {
        imgWidth = 750
    }
    if (location.href.indexOf('=') !== -1) {
        var titleAnd = 50;
        if (location.href.indexOf('&') !== -1) {
            titleAnd = location.href.indexOf('&');
            var changTitle = location.href.substr(titleAnd + 7) || '相册详情';
            topTitle = document.title = decodeURI(changTitle);
        }
        var albumId = location.href.substring(location.href.indexOf('=') + 1, titleAnd);
        $.ajax({
            url: serverHost + '/upload/v2/list?albumId=' + albumId + '&orgId=' + getOrigId(),
            type: 'GET',
            headers: { 'userId': id },
            success: function (res) {
                if (res.code === 200) {
                    if (res.data.list != '') {
                        $(".empty_data").hide();
                        var $boxLeft = $('.box_left'),
                            $boxRight = $('.box_right'),
                            lHeight = 0,
                            rHeight = 0;
                        resData = res.data.list;
                        pic_nums = resData.length;
                        $container.css({'width': (pic_nums * 100 + '%')});
                        for (var i = 0; i < resData.length; i++) {
                            var obj =  {
                                url: resData[i].filePath + '?x-oss-process=image/resize,w_' + imgWidth,
                                memo: resData[i].remark || ''
                            };
                            imgUrl.push(obj);
                            var img = '<a href="javascript:void(0)" target="_blank"><img class="lazy" data-index="' + i + '" data-original="' + resData[i].filePath + '?x-oss-process=image/resize,w_' + imgWidth + '" alt=""></a>';
                            if (rHeight >= lHeight) {
                                lHeight += resData[i].height * imgWidth/2/resData[i].width;
                                $boxLeft.append(img);
                            } else {
                                rHeight += resData[i].height * imgWidth/2/resData[i].width;
                                $boxRight.append(img);
                            }
                            // 照片详情渲染
                            $container.append(img);
                            $("img.lazy").lazyload({effect: "fadeIn"});
                        }
                    } else {
                        $(".empty_data").show();
                    }
                }else {
                    showTips('暂无权限访问')
                }
            }
        }).fail(function () {
            window.open("../feed_back/feed_back.html",'top')
        });

    }


});
// 照片详情
if(MGNative){
    MGNative.setupWebViewJavascriptBridge(function (bridge) {
        $(document).on('click', '.box img', function () {
            var _this = $(this);
            if (_this.attr('src')) {
                MGNative.previewImage(bridge, {
                    title: topTitle,
                    current: _this.attr('src'),
                    urls: imgUrl
                });
            }
        });
    });

}
