package com.mango.photoalbum.controller;

import com.mango.photoalbum.utils.ArithUtils;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.DateUtils;
import com.mango.photoalbum.utils.OSUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import oshi.hardware.CentralProcessor;
import oshi.hardware.GlobalMemory;
import oshi.hardware.HardwareAbstractionLayer;
import oshi.software.os.FileSystem;
import oshi.software.os.OSFileStore;
import oshi.software.os.OperatingSystem;
import oshi.SystemInfo;
import oshi.util.Util;
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
        SystemInfo si = new SystemInfo();
        HardwareAbstractionLayer hal = si.getHardware();
        setSysInfo(request, result);
        setCpuInfo(hal.getProcessor(), result);
        setMemInfo(hal.getMemory(), result);
        setJvmInfo(result);
        setSysFiles(si.getOperatingSystem(), result);
        return result;
    }

    /**
     * 服务器CPU信息
     */
    private void setCpuInfo(CentralProcessor processor, Map<String, Object> result) {
        Map<String, Object> cpuInfo = new LinkedHashMap<>();
        long[] prevTicks = processor.getSystemCpuLoadTicks();
        Util.sleep(1000);
        long[] ticks = processor.getSystemCpuLoadTicks();
        long nice = ticks[CentralProcessor.TickType.NICE.getIndex()] - prevTicks[CentralProcessor.TickType.NICE.getIndex()];
        long irq = ticks[CentralProcessor.TickType.IRQ.getIndex()] - prevTicks[CentralProcessor.TickType.IRQ.getIndex()];
        long softirq = ticks[CentralProcessor.TickType.SOFTIRQ.getIndex()] - prevTicks[CentralProcessor.TickType.SOFTIRQ.getIndex()];
        long steal = ticks[CentralProcessor.TickType.STEAL.getIndex()] - prevTicks[CentralProcessor.TickType.STEAL.getIndex()];
        long cSys = ticks[CentralProcessor.TickType.SYSTEM.getIndex()] - prevTicks[CentralProcessor.TickType.SYSTEM.getIndex()];
        long user = ticks[CentralProcessor.TickType.USER.getIndex()] - prevTicks[CentralProcessor.TickType.USER.getIndex()];
        long iowait = ticks[CentralProcessor.TickType.IOWAIT.getIndex()] - prevTicks[CentralProcessor.TickType.IOWAIT.getIndex()];
        long idle = ticks[CentralProcessor.TickType.IDLE.getIndex()] - prevTicks[CentralProcessor.TickType.IDLE.getIndex()];
        float totalCpu = user + nice + cSys + idle + iowait + irq + softirq + steal;
        cpuInfo.put("核心数", processor.getLogicalProcessorCount() + "个");
        cpuInfo.put("CPU总的使用率", ArithUtils.mul(totalCpu, 100, 2) + "%");
        cpuInfo.put("CPU系统使用率", ArithUtils.mul(cSys, 100, 2) + "%");
        cpuInfo.put("CPU用户使用率", ArithUtils.mul(user, 100, 2) + "%");
        cpuInfo.put("CPU当前等待率", ArithUtils.mul(iowait, 100, 2) + "%");
        cpuInfo.put("CPU当前空闲率", ArithUtils.mul(idle, 100, 2) + "%");
        result.put("CPU信息", cpuInfo);
    }

    /**
     * 服务器内存相关信息
     * @param memory
     * @param result
     */
    private void setMemInfo(GlobalMemory memory, Map<String, Object> result) {
        Map<String, Object> memInfo = new LinkedHashMap<>();
        double total = ArithUtils.divide(memory.getTotal(), (1024 * 1024 * 1024), 2);
        double free = ArithUtils.divide(memory.getAvailable(), (1024 * 1024 * 1024), 2);
        double used = total - free;
        memInfo.put("内存总量", total + "G");
        memInfo.put("已用内存", used + "G");
        memInfo.put("剩余内存", free + "G");
        memInfo.put("使用率", ArithUtils.multiply(ArithUtils.divide(used, total, 4), 100) + "%");
        result.put("服务器内存", memInfo);
    }

    /**
     * Java虚拟机信息
     */
    private void setJvmInfo(Map<String, Object> result) {
        Map<String, Object> jvmInfo = new LinkedHashMap<>();
        Properties props = System.getProperties();
        double total = ArithUtils.divide(Runtime.getRuntime().totalMemory(), (1024 * 1024), 2);
        double max = ArithUtils.divide(Runtime.getRuntime().maxMemory(), (1024 * 1024), 2);
        double free = ArithUtils.divide(Runtime.getRuntime().freeMemory(), (1024 * 1024), 2);
        jvmInfo.put("JVM内存总量", total + "M");
        jvmInfo.put("JVM最大可用内存", max + "M");
        jvmInfo.put("JVM空闲内存", free + "M");
        jvmInfo.put("JDK版本", props.getProperty("java.version"));
        jvmInfo.put("JDK路径", props.getProperty("java.home"));
        jvmInfo.put("JDK名字", ManagementFactory.getRuntimeMXBean().getVmName());
        jvmInfo.put("JDK启动时间", DateUtils.dateToStr(new Date(ManagementFactory.getRuntimeMXBean().getStartTime()),"yyyy-MM-dd HH:mm:ss"));
        jvmInfo.put("JDK运行时间", DateUtils.datePoor(new Date(), new Date(ManagementFactory.getRuntimeMXBean().getStartTime())));
        result.put("JVM信息", jvmInfo);
    }

    /**
     * 磁盘信息
     */
    private void setSysFiles(OperatingSystem os, Map<String, Object> result) {
        FileSystem fileSystem = os.getFileSystem();
        OSFileStore[] fsArray = fileSystem.getFileStores();
        List<Map<String, Object>> sysFiles = new ArrayList<>();
        for (OSFileStore fs : fsArray) {
            Map<String, Object> sysFile = new LinkedHashMap<>();
            long free = fs.getUsableSpace();
            long total = fs.getTotalSpace();
            long used = total - free;
            sysFile.put("盘符路径", fs.getMount());
            sysFile.put("盘符类型", fs.getType());
            sysFile.put("文件类型", fs.getName());
            sysFile.put("总大小", convertFileSize(total));
            sysFile.put("剩余大小", convertFileSize(free));
            sysFile.put("已经使用量", convertFileSize(used));
            sysFile.put("资源的使用率", ArithUtils.multiply(ArithUtils.divide(used, total, 4), 100));
            sysFiles.add(sysFile);
        }
        result.put("磁盘信息", sysFiles);
    }

    /**
     * 服务器信息
     */
    private void setSysInfo(HttpServletRequest request, Map<String, Object> result) {
        Properties props = System.getProperties();
        result.put("项目版本号", varsion);
        result.put("访问者IP", CommonUtils.getIpAddr(request));
        result.put("服务器Host", OSUtils.localHostName());
        result.put("服务器IP", OSUtils.localIP());
        result.put("操作系统", props.getProperty("os.name"));
        result.put("系统架构", props.getProperty("os.arch"));
        result.put("项目路径", props.getProperty("user.dir"));
    }

    /**
     * 字节转换
     * @param size 字节大小
     * @return 转换后值
     */
    private String convertFileSize(long size) {
        long kb = 1024;
        long mb = kb * 1024;
        long gb = mb * 1024;
        if (size >= gb) {
            return String.format("%.1f GB", (float) size / gb);
        } else if (size >= mb) {
            float f = (float) size / mb;
            return String.format(f > 100 ? "%.0f MB" : "%.1f MB", f);
        } else if (size >= kb) {
            float f = (float) size / kb;
            return String.format(f > 100 ? "%.0f KB" : "%.1f KB", f);
        } else {
            return String.format("%d B", size);
        }
    }

}
