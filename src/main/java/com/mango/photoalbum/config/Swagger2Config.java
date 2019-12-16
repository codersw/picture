package com.mango.photoalbum.config;


import com.mango.photoalbum.annotation.ApiVersion;
import com.mango.photoalbum.constant.ApiVersionConstant;
import com.mango.photoalbum.utils.CommonUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;
import java.util.Arrays;

/**
 *  swagger配置
 * @Author: swen
 */
@Configuration
@EnableSwagger2
public class Swagger2Config {

    /**
     * 默认分组
     * 不包括有注解标明版本号的
     * @return
     */
    @Bean
    @SuppressWarnings("deprecation")
    public Docket createRestApi() {
        return new Docket(DocumentationType.SWAGGER_2)
                .apiInfo(apiInfo())
                .select()
                .apis(RequestHandlerSelectors.basePackage("com.mango.photoalbum"))
                .apis(p -> {
                    assert p != null;
                    ApiVersion apiVersion = p.declaringClass().getAnnotation(ApiVersion.class);
                    if(CommonUtils.isNullOrEmpty(apiVersion)) {
                        apiVersion = p.getHandlerMethod().getMethodAnnotation(ApiVersion.class);
                    }
                    return apiVersion == null;
                })
                .paths(PathSelectors.any())
                .build();
    }

    /**
     * v1分组
     * 只读取注解版本号v1的
     * @return
     */
    @Bean
    @SuppressWarnings("deprecation")
    public Docket v1(){
        return new Docket(DocumentationType.SWAGGER_2)
                .apiInfo(apiInfo())
                .groupName(ApiVersionConstant.V1)
                .select()
                .apis(RequestHandlerSelectors.basePackage("com.mango.photoalbum"))
                .apis(p -> {
                    assert p != null;
                    ApiVersion apiVersion = p.declaringClass().getAnnotation(ApiVersion.class);
                    if(CommonUtils.isNullOrEmpty(apiVersion)) {
                        apiVersion = p.getHandlerMethod().getMethodAnnotation(ApiVersion.class);
                    }
                    return apiVersion != null && Arrays.asList(apiVersion.value()).contains(ApiVersionConstant.V1);
                })
                .paths(PathSelectors.any())
                .build();
    }

    /**
     * api信息
     * @return
     */
    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("相册 API文档")
                //创建人
                .contact(new Contact("swen", "", ""))
                .description("API 描述")
                .version("1.0")
                .build();
    }
}
