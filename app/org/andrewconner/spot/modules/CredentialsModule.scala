package org.andrewconner.spot.modules

import com.amazonaws.auth.{ BasicAWSCredentials, AWSCredentials }
import org.andrewconner.spot.AppComponents
import play.api.{ Configuration, Application }

trait CredentialsModule { self: AppComponents =>

  def awsCredentials: AWSCredentials = {
    (for {
      accessKey <- config("aws.credentials.accessKey")
      secretKey <- config("aws.credentials.secretKey")
    } yield new BasicAWSCredentials(accessKey, secretKey)).get
  }

  private def config(key: String) = configuration.getString(key, None)
}
