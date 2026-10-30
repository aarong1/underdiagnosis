footer <- function(){
  HTML('<!-- Footer -->
                                 <footer class="dashboard-footer">
                                 <div class="footer-content">
                                 <div class="footer-info">
                                 <p>&copy;  Obesity - Data runs of the Population health Models microsimulation, data are estimates </p>
                                 <p>Data is updated periodically when underlying datasets are released from their respective providers </p>
                                 </div>
                                 <div class="footer-links">
                                 <p> Public Health Agency - Population Health Model 2025  </p>
                                 <!--<a href="#methodology">Methodology</a>
                                 <a href="#privacy">Privacy Policy</a>
                                 <a href="#contact">Contact</a> -->
                                 </div>
                                 </div>
                                 <!--<img src = "pha.png" alt = "PHA Logo" style="height:40px;position:absolute;right:15px;top:10px;"> -->
                                </footer>
                                 <style>
                                 .dashboard-footer {
                                 margin-inline:-12px;
                                 width:100vw;
                                   position:relative;
                                   display:fixed;
                                   bottom: 0;
                                   background: #f8f9fa;
                                   padding: 10px 15px;
                                   border-top: 1px solid #e9ecef;
                                   margin-top: 20px;
                                 }

                                 .footer-content {
                                   display: flex;
                                   justify-content: space-between;
                                   align-items: center;
                                   flex-wrap: wrap;
                                 }
                                 .footer-info p {
                                   margin: 0;
                                   font-size: 0.9em;
                                   color: #6c757d;
                                 }
                                 .footer-links a {
                                   margin-left: 15px;
                                   font-size: 0.9em;
                                   color: #007bff;
                                   text-decoration: none;
                                 }
                                 .footer-links a:hover {
                                   text-decoration: underline;
                                 }
                                 @media (max-width: 600px) {
                                   .footer-content {
                                     flex-direction: column;
                                     align-items: flex-start;
                                   }
                                   .footer-links {
                                     margin-top: 10px;
                                   }
                                   .footer-links a {
                                     margin-left: 0;
                                     margin-right: 15px;
                                   }
                                 }
                                 </style>')}