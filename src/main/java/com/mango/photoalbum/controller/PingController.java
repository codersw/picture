package com.mango.photoalbum.controller;

import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.OSUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

/**
 * 健康检测接口
 * @author swen
 */
@Api(value = "健康检测接口", tags = {"健康检测接口"})
@RestController
@RequestMapping("/ping")
public class PingController {

    @Value("${project.version}")
    private String varsion;

    @ApiOperation(value = "获取ip", notes = "获取ip")
    @RequestMapping(method = { RequestMethod.GET, RequestMethod.HEAD})
    public Object ping(HttpServletRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("version", varsion);
        result.put("reqestIP", CommonUtils.getIpAddr(request));
        result.put("localHostName", OSUtils.localHostName());
        result.put("localIP", OSUtils.localIP());
        result.put("cpuUsage", OSUtils.cpuUsage());
        result.put("memoryUsage", OSUtils.memoryUsage());
        result.putAll(OSUtils.cpuinfo());
        return result;
    }
}
