package com.mango.photoalbum.controller;


import com.mango.photoalbum.utils.CommonUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

@Api(value = "健康检测接口", tags = {"健康检测接口"})
@RestController
@RequestMapping("ping")
public class PingController {

    @ApiOperation(value = "获取ip", notes = "获取ip")
    @RequestMapping(method = { RequestMethod.GET, RequestMethod.HEAD})
    public String ping(HttpServletRequest request) {
        return CommonUtils.getIpAddr(request);
    }
}
