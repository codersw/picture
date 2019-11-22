package com.mango.picture.config;

import java.util.concurrent.*;

/**
 * 线程池帮助类
 */
public class ThreadPoolHelper {

    private int poolSize;
    private int capacity;

    public ThreadPoolHelper() {
        this(1);
    }

    /**
     * 构造函数
     * @param poolSize 线程池大小
     */
    public ThreadPoolHelper(int poolSize) {
        this(poolSize, 512);
    }

    /**
     * 构造函数
     * @param poolSize 线程池大小
     * @param capacity 队列长度
     */
    public ThreadPoolHelper(int poolSize, int capacity) {
       this.poolSize = poolSize;
       this.capacity = capacity;
    }

    /**
     * 执行方法
     * @param task 待执行事件
     */
    public void Executor(Runnable task) {
        try {
            BlockingQueue<Runnable> queue = new ArrayBlockingQueue<>(capacity);
            RejectedExecutionHandler policy = new ThreadPoolExecutor.DiscardPolicy();
            ExecutorService executorService = new ThreadPoolExecutor(poolSize, poolSize,
                    0, TimeUnit.SECONDS,
                    queue,
                    policy);
            executorService.submit(task);
            executorService.shutdown();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
