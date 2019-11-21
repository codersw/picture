package com.mango.picture.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/sample")
@RefreshScope
class SampleController {

    @Value("${alibaba.oss.httpPrefix}")
    private String httpPrefix;

    @GetMapping("/acm")
    public String simple() {
        return httpPrefix;
    }
}
