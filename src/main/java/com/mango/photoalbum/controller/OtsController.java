package com.mango.photoalbum.controller;

import com.mango.photoalbum.annotation.RequiredPermission;
import com.mango.photoalbum.constant.PermissionConst;
import com.mango.photoalbum.model.Result;
import com.mango.photoalbum.model.ResultGenerator;
import com.mango.photoalbum.utils.OtsUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Resource;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Api(value = "表格信息接口", tags = {"表格信息接口"})
@Slf4j
@RestController
@RequestMapping("/ots")
@RequiredPermission(PermissionConst.SUPPERUSERFLAGENUM)
public class OtsController {

    @Resource
    private OtsUtils ots;

    /**
     * ots表格列表
     * @return
     */
    @ApiOperation(value = "表格列表", notes = "表格列表")
    @GetMapping
    public Result listTable() {
        return ResultGenerator.genSuccessResult(ots.listTable());
    }

    /**
     * 表格创建
     * @return
     */
    @ApiOperation(value = "表格创建", notes = "表格创建")
    @GetMapping("/table/{modelName}")
    public Result createTable(@PathVariable String modelName) {
        ots.creatTable(toClass(modelName));
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 表格删除
     * @return
     */
    @ApiOperation(value = "表格删除", notes = "表格删除")
    @DeleteMapping("/table/{modelName}")
    public Result deleteTable(@PathVariable String modelName) {
        ots.deleteTable(toClass(modelName));
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 列出多元索引
     * @return
     */
    @ApiOperation(value = "列出多元索引", notes = "列出多元索引")
    @GetMapping("/{modelName}")
    public Result listSearchIndex(@PathVariable String modelName) {

        return ResultGenerator.genSuccessResult(ots.describeSearchIndex(toClass(modelName)));
    }

    /**
     * 创建多元索引
     * @return
     */
    @ApiOperation(value = "创建多元索引", notes = "创建多元索引")
    @GetMapping("/searchIndex/{modelName}")
    public Result createSearchIndex(@PathVariable String modelName) {
        ots.createSearchIndex(toClass(modelName));
        return ResultGenerator.genSuccessResult();
    }

    /**
     * 删除多元索引
     * @return
     */
    @ApiOperation(value = "删除多元索引", notes = "删除多元索引")
    @DeleteMapping("/searchIndex/{modelName}")
    public Result deleteSearchIndex(@PathVariable String modelName) {
        ots.deleteSearchIndex(toClass(modelName));
        return ResultGenerator.genSuccessResult();
    }

    /**
     * model名字转class
     * @param modelName
     * @return
     */
    @SneakyThrows
    private Class<?> toClass(String modelName) {
        if(modelName.contains("_")) {
            modelName = lineToHump(modelName);
        }
        return Class.forName("com.mango.photoalbum.model." + modelName);
    }

    /**
     * 线转驼峰首字母并大写
     * @param str
     * @return
     */
    private String lineToHump(String str) {
        str = str.toLowerCase();
        Matcher matcher = Pattern.compile("_(\\w)").matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, matcher.group(1).toUpperCase());
        }
        matcher.appendTail(sb);
        return sb.substring(0, 1).toUpperCase() + sb.substring(1);
    }
}
