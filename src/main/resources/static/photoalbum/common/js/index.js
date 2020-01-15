
var doingFlag = false,// 上拉加载标识
    timeoutId;

// 获取相册列表
function getListData(params) {
    if (params.pageIndex !== 1) {
        $(".loading").show();
    }
    $.ajax({
        url: serverHost + '/album/v1/list',
        type: 'GET',
        data: params,
        headers: { 'userId': id},
        success: function (res) {
            var html = '',
                $list = $(".list");
            if (res.code === 200 && res.data) {
                if (res.data.list != '') {
                    $(".empty_data").hide();
                    for (var i = 0, len = res.data.list.length; i < len; i++) {
                        // 显示添加时间，如果有修改时间，显示修改时间
                        if (res.data.list[i].modifyTime || res.data.list[i].createTime) {
                            res.data.list[i].createTime = res.data.list[i].modifyTime.substr(0, 10) || res.data.list[i].createTime.substr(0, 10);
                        }
                        html += '<a target="_blank" href="./views/album_details/album_details.html?id=' + res.data.list[i].albumId + '&title=' + res.data.list[i].title + '" class="list_cell">\n' +
                            '            <div class="left_img common_img">' +
                            '                <div class="img_bg" style="background-image: url(' + (res.data.list[i].coverPath ? res.data.list[i].coverPath + "?x-oss-process=image/resize,h_200,w_200" : "https://g.517cdn.com/www517cn/2016v1/images/noimg_small.png") + ')">' +
                            '                </div>' +
                            '            </div>\n' +
                            '            <div class="right_msg">\n' +
                            '                <div class="title">\n' +
                            '                    ' + res.data.list[i].title + '\n' +
                            '                </div>\n' +
                            '                <p>更新时间：<span> ' + (res.data.list[i].createTime || '未知') + '</span></p>\n' +
                            '            </div>\n' +
                            '        </a>'
                    }
                    $list.append(html);
                    var noimg = document.getElementsByTagName("img");
                    for(var p = 0; p < noimg.length; p++){
                        noimg[p].onerror = function(){
                            this.src="https://g.517cdn.com/www517cn/2016v1/images/noimg_small.png"
                        }
                    }
                } else {
                    if (params.pageIndex === 1) {
                        // 空数组
                        $(".empty_data").show();
                    } else {
                        showTips('暂无更多数据');
                        doingFlag = true;
                    }

                }
                $(".loading").hide();
            }
        }
    }).fail(function () {
        showTips('服务器错误');
    })
}