package com.mango.photoalbum.controller;

import com.mango.photoalbum.utils.CookieUtil;
import io.swagger.models.auth.In;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import springfox.documentation.annotations.ApiIgnore;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

@RestController
@ApiIgnore
public class TestController {

    @Resource
    private RestTemplate restTemplate;

    @GetMapping("/test")
    public void test(HttpServletResponse response) {
        CookieUtil.set(response,"newframeuid", "11877", Integer.MAX_VALUE);
    }

    @GetMapping("/mangoapi/UserNoLogin/GetUserName")
    public Object mangoapi(Integer userid) {
        return restTemplate.getForObject("https://mis.517.cn/mangoapi/UserNoLogin/GetUserName?userid=" + userid, Object.class);
    }
}
