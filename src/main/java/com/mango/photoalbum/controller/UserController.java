package com.mango.photoalbum.controller;

import com.alibaba.fastjson.JSONObject;
import com.mango.photoalbum.model.Result;
import com.mango.photoalbum.model.ResultGenerator;
import com.mango.photoalbum.utils.CookieUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import springfox.documentation.annotations.ApiIgnore;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

@Api(value = "获取用户信息", tags= "获取用户信息")
@RestController
@RequestMapping("/user")
public class UserController {

    @Resource
    private RestTemplate restTemplate;

    @ApiIgnore
    @GetMapping("/test")
    public void test(HttpServletResponse response) {
        CookieUtil.set(response,"newframeuid", "11877", Integer.MAX_VALUE);
        CookieUtil.set(response,"userId", "11877", Integer.MAX_VALUE);
    }

    /**
     * 获取用户信息
     * @param userId
     * @return
     */
    @ApiOperation(value = "获取用户信息", notes = "获取用户信息")
    @GetMapping("/{userId}")
    public Result mangoapi(@PathVariable Integer userId) {
        JSONObject json = restTemplate.getForObject("https://mis.517.cn/mangoapi/UserNoLogin/GetUserName?userid=" + userId, JSONObject.class);
        assert json != null;
        if(json.getInteger("flag").equals(100)) {
            Map<String, Object> result = new HashMap<>();
            JSONObject userInfo = json.getJSONObject("result");
            result.put("userName", userInfo.getString("UserName"));
            result.put("userId", userInfo.getInteger("userid"));
            result.put("userPhoto", userInfo.getString("userphoto"));
            result.put("orgName", userInfo.getString("OrgName"));
            String orgId = userInfo.getString("OrgId");
            String orgIdAll = userInfo.getString("OrgParentIDALL");
            orgIdAll = orgIdAll.replaceAll("\\|", "");
            result.put("orgId", orgId);
            result.put("orgIdAll", orgIdAll);
            return ResultGenerator.genSuccessResult(result);
        } else {
            return ResultGenerator.genFailResult(json.getString("message"));
        }
    }
}
