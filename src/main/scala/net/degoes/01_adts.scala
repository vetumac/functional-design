package net.degoes

import java.time.Instant

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  final case class Number(value: String)
  final case class Name(value: String)
  final case class ExpirationDate(month: Int, year: Int)
  final case class SecurityCode(value: String)
  final case class CreditCard(
    number: Number,
    name: Name,
    expirationDate: ExpirationDate,
    securityCode: SecurityCode
  )

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product
  object Product {
    sealed trait Physical extends Product
    object Physical {
      final case class Milk() extends Physical
    }

    sealed trait Digital extends Product
    object Digital {
      final case class Book()  extends Digital
      final case class Movie() extends Digital
    }

    sealed trait Access extends Product
    object Access {
      final case class Event()        extends Access
      final case class MusicConcert() extends Access
      final case class FilmShowing()  extends Access
    }
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme
  object PricingScheme {
    case class Regular(amount: BigDecimal, expire: Instant)   extends PricingScheme
    case class Recurring(amount: BigDecimal, dayOfMonth: Int) extends PricingScheme
  }

  /**
   * EVENT PROCESSING - EXERCISE SET 3
   *
   * Consider an event processing application, which processes events from both
   * devices, as well as users.
   */
  object events {

    /**
     * EXERCISE
     *
     * Refactor the object-oriented data model in this section to a more
     * functional one, which uses only sealed traits and case classes.
     */
    final case class Event(id: Int, time: Instant, payload: EventPayload)

    sealed trait EventPayload
    object EventPayload {
      final case class User(userName: String, details: UserEventDetails)  extends EventPayload
      final case class Device(deviceId: Int, details: DeviceEventDetails) extends EventPayload

      sealed trait UserEventDetails
      object UserEventDetails {
        final case class Purchased(item: String, price: Double) extends UserEventDetails
        case object AccountCreated                              extends UserEventDetails
      }

      sealed trait DeviceEventDetails
      object DeviceEventDetails {
        final case class Updated(reading: Option[Double]) extends DeviceEventDetails
        case object Activated                             extends DeviceEventDetails
      }
    }

    /**
     * DOCUMENT EDITING - EXERCISE SET 4
     *
     * Consider a web application that allows users to edit and store documents
     * of some type (which is not relevant for these exercises).
     */
    object documents {
      final case class UserId(identifier: String)
      final case class DocId(identifier: String)
      final case class DocContent(body: String)

      /**
       * EXERCISE 1
       *
       * Using only sealed traits and case classes, create a simplified but somewhat
       * realistic model of a Document.
       */
      final case class Document(userId: UserId, docId: DocId, docContent: DocContent)

      /**
       * EXERCISE 2
       *
       * Using only sealed traits and case classes, create a model of the access
       * type that a given user might have with respect to a document. For example,
       * some users might have read-only permission on a document.
       */
      sealed trait AccessType
      object AccessType {
        case object FullAccess
        case object Edit
        case object ReadOnly
      }
    }

    /**
     * EXERCISE 3
     *
     * Using only sealed traits and case classes, create a model of the
     * permissions that a user has on a set of documents they have access to.
     * Do not store the document contents themselves in this model.
     */
    import documents._
    final case class DocPermissions(userId: UserId, accesses: (DocId, AccessType))
  }

  /**
   * BANKING - EXERCISE SET 5
   *
   * Consider a banking application that allows users to hold and transfer money.
   */
  object bank {

    /**
     * EXERCISE 1
     *
     * Using only sealed traits and case classes, develop a model of a customer at a bank.
     */
    final case class Customer(name: String)

    /**
     * EXERCISE 2
     *
     * Using only sealed traits and case classes, develop a model of an account
     * type. For example, one account type allows the user to write checks
     * against a given currency. Another account type allows the user to earn
     * interest at a given rate for the holdings in a given currency.
     */
    sealed trait AccountType
    object AccountType {
      final case class Casual(currency: Currency)                   extends AccountType
      final case class Saving(currency: Currency, rate: BigDecimal) extends AccountType

      sealed trait Currency
      object Currency {
        case object Eur extends Currency
        case object Usd extends Currency
      }
    }
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  import bank._
  final case class Account(accountType: AccountType, value: BigDecimal, owner: Customer, hasAccess: Customer)
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  sealed trait Exchange
  object Exchange {
    case object Nasdaq extends Exchange
    case object Nyse   extends Exchange
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  sealed trait CurrencyType
  object CurrencyType {
    case object Eur extends CurrencyType
    case object Usd extends CurrencyType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  sealed trait StockSymbol
  object StockSymbol {
    case object Appl extends StockSymbol
    case object Evo  extends StockSymbol
  }

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  final case class Portfolio(shares: List[(StockSymbol, Long)], moneys: List[(CurrencyType, BigDecimal)])

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  final case class User(name: String, portfolio: Portfolio)

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  sealed trait TradeType
  object TradeType {
    final case class Buy(exchange: Exchange)  extends TradeType
    final case class Sell(exchange: Exchange) extends TradeType
  }

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  final case class Trade(tradeType: TradeType, stockSymbol: StockSymbol, price: BigDecimal)
}
