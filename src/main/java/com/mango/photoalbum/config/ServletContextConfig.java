package com.mango.photoalbum.config;

import com.mango.photoalbum.Interceptor.AuthorizationInterceptor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurationSupport;
import javax.annotation.Resource;

/**
 * 拦截器配置
 */
@Configuration
public class ServletContextConfig extends WebMvcConfigurationSupport {

    @Resource
    private AuthorizationInterceptor authorizationInterceptor;

    /**
     * 发现如果继承了WebMvcConfigurationSupport，则在yml中配置的相关内容会失效。
     * 需要重新指定静态资源
     * @param registry
     */
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("swagger-ui.html").addResourceLocations("classpath:/META-INF/resources/");
        registry.addResourceHandler("**").addResourceLocations("classpath:/static/");
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
