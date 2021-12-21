/*
 * Copyright 2012-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ehens86.bet.ncaa_fb_sb_odds_parse_api;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.StartService;

//@Configuration
//@EnableAutoConfiguration
//@ComponentScan

@SpringBootApplication
//@ImportResource("classpath:applicationContext.xml")
@EnableJpaRepositories(basePackages = "com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository")
@EntityScan("com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain")
public class App implements CommandLineRunner {
	private static final Logger LOG = Logger.getLogger(App.class.toString());

	// Simple example shows how a command line spring application can execute an
	// injected bean service. Also demonstrates how you can use @Value to inject
	// command line args ('--name=whatever') or application properties

	@Autowired
	private StartService startService;

	@Override
	public void run(String... args) {
		String startMessage = this.startService.getHelloMessage();
		LOG.log(Level.INFO, startMessage);
	}

	public static void main(String[] args) {
		SpringApplication.run(App.class, args);
	}
}
