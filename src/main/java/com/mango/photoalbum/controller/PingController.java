package com.mango.photoalbum.controller;

import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.DateUtils;
import com.mango.photoalbum.utils.OSUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import javax.servlet.http.HttpServletRequest;
import java.lang.management.ManagementFactory;
import java.util.*;

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

    /**
     * 健康检测接口
     * @param request
     * @return
     */
    @ApiOperation(value = "健康检测接口", notes = "健康检测接口")
    @RequestMapping(method = { RequestMethod.GET, RequestMethod.HEAD})
    public Map ping(HttpServletRequest request) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("项目版本号", varsion);
        result.put("访问者IP", CommonUtils.getIpAddr(request));
        result.put("服务器Host", OSUtils.localHostName());
        result.put("服务器IP", OSUtils.localIP());
        result.put("内存使用率", OSUtils.memoryUsage());
        Properties props = System.getProperties();
        result.put("JVM占用的内存总数(M)", Runtime.getRuntime().totalMemory());
        result.put("JVM最大可用内存总数(M)", Runtime.getRuntime().maxMemory());
        result.put("JVM空闲内存(M)", Runtime.getRuntime().freeMemory());
        result.put("JDK版本", props.getProperty("java.version"));
        result.put("JDK路径", props.getProperty("java.home"));
        result.put("JDK名字", ManagementFactory.getRuntimeMXBean().getVmName());
        result.put("JDK启动时间", DateUtils.dateToStr(new Date(ManagementFactory.getRuntimeMXBean().getStartTime()),"yyyy-MM-dd HH:mm:ss"));
        result.put("JDK运行时间", DateUtils.datePoor(new Date(), new Date(ManagementFactory.getRuntimeMXBean().getStartTime())));
        result.put("CPU信息", OSUtils.cpuinfo());
        return result;
    }
}
