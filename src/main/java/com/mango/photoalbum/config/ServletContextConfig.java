package com.mango.photoalbum.config;

import com.mango.photoalbum.Interceptor.AuthorizationInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.*;
import javax.annotation.Resource;

/**
 * 静态资源配置
 * @author swen
 */
@Configuration
public class ServletContextConfig extends WebMvcConfigurationSupport {

    @Resource
    private AuthorizationInterceptor authorizationInterceptor;

    @Value("${spring.profiles.active}")
    private String active;

    /**
     * 发现如果继承了WebMvcConfigurationSupport，则在yml中配置的相关内容会失效。
     * 需要重新指定静态资源
     * @param registry
     */
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/**").addResourceLocations("classpath:/static/");
        //生产环境不暴露swagger
        if(!active.contains("prod")) {
            registry.addResourceHandler("swagger-ui.html").addResourceLocations("classpath:/META-INF/resources/");
            registry.addResourceHandler("/webjars/**").addResourceLocations("classpath:/META-INF/resources/webjars/");
        }
        super.addResourceHandlers(registry);
    }

    /**
     *  加入的顺序就是拦截器执行的顺序，
     *  按顺序执行所有拦截器的preHandle
     *  所有的preHandle 执行完再执行全部postHandle 最后是postHandle
     * @param registry
     */
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(authorizationInterceptor).addPathPatterns("/**");
    }

    /**
     * 配置servlet处理
     */
    @Override
    public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
        configurer.enable();
    }
}
