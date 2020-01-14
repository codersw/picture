var imgUrl = [],
    topTitle = '我的照片';
$(function () {
    var $dateList = $(".date_list"),
        $generalList = $(".general_list"),
        $operateI = $(".operate_cell i"),
        showFlag = 1,
        reqFlag = true;

    // 切换显示方式
    $(".sort").on('click', function () {
        $dateList.show();
        $generalList.hide();
        $operateI.removeClass('active');
        $(".sort i").addClass('active');
        showFlag = 1;
    });
    $(".display").on('click', function () {
        $dateList.hide();
        $generalList.show();
        $operateI.removeClass('active');
        $(".display i").addClass('active');
        showFlag = 2;
    });


    // 增加空div的数量
    var addIndex = 0,  // 普通列表
        dateIndex = 0;  // 日期列表

    // 时间分类数据，用于存储返回的同日期数组
    var dateData = [];

    // 请求参数
    var params = {
        pageIndex: 1,
        pageSize: 20,
        type: 1,
        userId: id
    };

    // 请求数据
    if (reqFlag) {
        function getData(params) {
            for (var m = 0; m < addIndex; m++) {
                $(".general_box .list_cell:last-child").remove()
            }
            // 请求数据
            $.ajax({
                url: serverHost + '/upload/v2/list',
                type: 'get',
                headers: {'userId': id},
                data: params,
                success: function (res) {
                    if (res.code === 200) {
                        if (res.data.list != '') {


                            if (res.data.list.length < 20) {
                                reqFlag = false;
                                $(".underline").show();
                            }

                            var generalList = '';
                            res.data.list.forEach(function (item, index) {

                                var obj =  {
                                    url: item.filePath + '?x-oss-process=image/resize,h_200,w_200',
                                    memo: item.remark || ''
                                };
                                imgUrl.push(obj);
                                var midData = {
                                    date: '',
                                    url: []
                                };
                                item.createTime = item.createTime.replace('-', '年');
                                item.createTime = item.createTime.replace('-', '月');
                                item.createTime = item.createTime.replace(' ', '日');
                                item.createTime = item.createTime.substr(0, 11);
                                if (index === 0) {
                                    if (JSON.stringify(dateData).indexOf(item.createTime) === -1) {
                                        midData.date = item.createTime;
                                        midData.url.push(item.filePath);
                                        dateData.push(midData)
                                    }
                                }
                                if (index !== 0) {
                                    for (var k = 0; k < dateData.length; k++) {
                                        if (dateData[k] && dateData[k].date === item.createTime) {
                                            dateData[k].url.push(item.filePath);
                                        }
                                        if (JSON.stringify(dateData).indexOf(item.createTime) === -1) {
                                            midData.date = item.createTime;
                                            midData.url.push(item.filePath);
                                            dateData.push(midData);
                                        }
                                    }
                                }
                                // 判断是否需要懒加载   15为预计铺满屏幕值
                                if ($(".general_box img").length < 15) {
                                    generalList += ' <div class="list_cell common_img">' +
                                        '   <img src="'+ item.filePath +'?x-oss-process=image/resize,h_200,w_200" alt="">' +
                                        '</div>';
                                } else {
                                    generalList += ' <div class="list_cell common_img">' +
                                        '   <img class="lazy" data-original="'+ item.filePath +'?x-oss-process=image/resize,h_200,w_200" alt="">' +
                                        '</div>';
                                }

                            });
                            // 不做兼容小屏 iPhone5
                            addIndex = 3 - (res.data.list.length - addIndex) % 3;
                            if (addIndex === 3) {
                                addIndex = 0;
                            }
                            // 为保持样式需要填充div
                            for (var i = 0; i < addIndex; i++) {
                                generalList += '<div class="list_cell"></div>'
                            }
                            $(".general_box").append(generalList);
                            $("img.lazy").lazyload({effect: "fadeIn"});

                            var dateList = '';
                            for (var j = 0; j < dateData.length; j++) {
                                var dateImg = '';
                                for (var n = 0; n < dateData[j].url.length; n++) {
                                    dateImg += '<div class="img_box">\n' +
                                        '                        <div class="date_img common_img">\n' +
                                        '                            <img class="lazy" data-original="'+ dateData[j].url[n] +'?x-oss-process=image/resize,h_200,w_200">' +
                                        '                        </div>\n' +
                                        '                    </div>';
                                }
                                // 不做兼容小屏 iPhone5
                                dateIndex = 3 - dateData[j].url.length % 3;
                                if (dateIndex === 3) {
                                    dateIndex = 0;
                                }
                                // 为保持样式需要填充div
                                for (var i = 0; i < dateIndex; i++) {
                                    dateImg += '<div class="img_box">\n' +

                                        '                    </div>';
                                }
                                dateList += ' <div class="date_cell">\n' +
                                    '                <div class="date">\n' +
                                    dateData[j].date +
                                    '                </div>\n' +
                                    '                <div class="date_box">\n' +
                                    dateImg +
                                    '                </div>\n' +
                                    '            </div>'
                            }
                            $(".date_list").children().remove();
                            $(".date_list").append(dateList);
                            $("img.lazy").lazyload({effect: "fadeIn"});
                        } else {
                            if (params.pageIndex === 1) {
                                // 空数组
                                $(".empty_data").show();
                            }else {
                                reqFlag = false;
                                $(".underline").show();
                            }
                        }
                    }
                }
            })
        }

    }

    getData(params);


    var doingFlag = false;
    var timeoutId;

    // 上拉加载
    function callback1() {
        var _list = document.getElementsByClassName("list")[0];
        var top = _list.getBoundingClientRect().top;
        var windowHeight = window.screen.height;
        var cuHeight = $('.list').height();
        if (top && cuHeight <= windowHeight - top + 1000) {
            if (reqFlag) {
                loadMoreFn()
            }
        }
    }

    function callback2() {
        var list = document.getElementsByClassName("list")[1];
        var top = list.getBoundingClientRect().top;
        var windowHeight = window.screen.height;
        var cuHeight = $('.list').height();
        if (top && cuHeight < windowHeight + 600 - top) {
            if (reqFlag) {
                loadMoreFn()
            }
        }
    }

    window.addEventListener('scroll', function () {
        if (doingFlag) {
            return
        }
        if (timeoutId) {
            clearTimeout(timeoutId)
        }
        if (showFlag === 1) {
            timeoutId = setTimeout(callback1, 50)
        } else {
            timeoutId = setTimeout(callback2, 50)
        }
    }.bind(this), false);

    function loadMoreFn() {

        params.pageIndex++;
        getData(params);
    }


});
// 照片详情
if (MGNative) {
    MGNative.setupWebViewJavascriptBridge(function (bridge) {
        $(document).on('click', '.list_cell img', function () {
            var _this = $(this),
                currentUrl = _this.attr('src');
            if (currentUrl && currentUrl.indexOf('https') !== -1) {
                MGNative.previewImage(bridge, {
                    title: topTitle,
                    current: currentUrl,
                    urls: imgUrl
                });
            }
        });
        $(document).on('click', '.date_img img', function () {
            var _this = $(this),
                currentUrl = _this.attr('src');
            if (currentUrl && currentUrl.indexOf('http') !== -1) {
                MGNative.previewImage(bridge, {
                    title: topTitle,
                    current: currentUrl,
                    urls: imgUrl
                });
            }
        });
    });
}