package com.mango.photoalbum.Interceptor;

import com.mango.photoalbum.constant.TokenConstant;
import com.mango.photoalbum.exception.UnauthorizedException;
import com.mango.photoalbum.utils.CookieUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Slf4j
@Component
public class AuthorizationInterceptor implements HandlerInterceptor {


    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws RuntimeException {
        if (!(handler instanceof HandlerMethod)) {
            return true;
        }
        //验证cookie中token的有效性
        /*String token = CookieUtil.get(request, TokenConstant.TOKEN);
        if(StringUtils.isBlank(token)) {
            log.info("认证已失效，清重新登录！");
            throw new UnauthorizedException("认证已失效，清重新登录！");
        }*/
        return true;
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object handler, ModelAndView modelAndView) throws Exception {
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) throws Exception {
    }
}