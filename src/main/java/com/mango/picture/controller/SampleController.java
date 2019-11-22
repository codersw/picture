package com.mango.picture.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 文件接口
 * @author swen
 */
@Api(value = "ACM动态配置测试接口", tags = {"ACM动态配置测试接口"})
@RestController
@RequestMapping("/sample")
@RefreshScope
class SampleController {

    @Value("${alibaba.oss.httpPrefix}")
    private String httpPrefix;

    @ApiOperation(value = "ACM测试接口", notes = "ACM测试接口")
    @GetMapping("/acm")
    public String simple() {
        return httpPrefix;
    }
}
